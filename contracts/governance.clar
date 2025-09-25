;; Governance Contract - Voting and Proposal Management
;; Manages community proposals, voting, and democratic decision-making for TaxDAO

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u201))
(define-constant ERR-VOTING-PERIOD-ENDED (err u202))
(define-constant ERR-VOTING-PERIOD-ACTIVE (err u203))
(define-constant ERR-ALREADY-VOTED (err u204))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u205))
(define-constant ERR-PROPOSAL-NOT-APPROVED (err u206))
(define-constant ERR-INVALID-AMOUNT (err u207))
(define-constant ERR-INVALID-DURATION (err u208))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u209))
(define-constant ERR-MINIMUM-VOTES-NOT-MET (err u210))

;; Contract constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-VOTING-PERIOD u1008) ;; Minimum 7 days (144 blocks per day)
(define-constant MAX-VOTING-PERIOD u4032) ;; Maximum 28 days
(define-constant QUORUM-PERCENTAGE u20) ;; 20% of total voters required
(define-constant APPROVAL-THRESHOLD u51) ;; 51% approval required

;; Data variables
(define-data-var next-proposal-id uint u1)
(define-data-var total-voters uint u0)
(define-data-var tax-dao-contract (optional principal) none)
(define-data-var min-proposal-deposit uint u1000000000) ;; 1000 STX minimum deposit
(define-data-var governance-token-holders uint u0)

;; Proposal structure
(define-map proposals uint {
  proposer: principal,
  title: (string-utf8 200),
  description: (string-utf8 1000),
  recipient: principal,
  amount: uint,
  start-block: uint,
  end-block: uint,
  votes-for: uint,
  votes-against: uint,
  total-votes: uint,
  executed: bool,
  deposit: uint
})

;; Voter registry and voting power
(define-map voters principal { registered: bool, voting-power: uint })
(define-map proposal-votes { proposal-id: uint, voter: principal } { voted: bool, vote: bool, power: uint })

;; Proposal categories for better organization
(define-map proposal-categories uint (string-utf8 50))

;; Events for transparency
(define-private (emit-proposal-created (proposal-id uint) (proposer principal))
  (print { event: "proposal-created", proposal-id: proposal-id, proposer: proposer, timestamp: stacks-block-height })
)

(define-private (emit-vote-cast (proposal-id uint) (voter principal) (vote-choice bool) (power uint))
  (print { event: "vote-cast", proposal-id: proposal-id, voter: voter, vote: vote-choice, power: power, timestamp: stacks-block-height })
)

(define-private (emit-proposal-executed (proposal-id uint))
  (print { event: "proposal-executed", proposal-id: proposal-id, timestamp: stacks-block-height })
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-voter-info (voter principal))
  (map-get? voters voter)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? proposal-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-next-proposal-id)
  (var-get next-proposal-id)
)

(define-read-only (get-total-voters)
  (var-get total-voters)
)

(define-read-only (get-quorum-requirement)
  (/ (* (var-get total-voters) QUORUM-PERCENTAGE) u100)
)

(define-read-only (is-proposal-approved (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal (let ((approval-votes (get votes-for proposal))
                   (total-votes (get total-votes proposal)))
               (and (>= total-votes (get-quorum-requirement))
                    (>= (* approval-votes u100) (* total-votes APPROVAL-THRESHOLD))))
    false
  )
)

(define-read-only (get-proposal-status (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal (let ((current-block stacks-block-height)
                   (voting-ended (>= current-block (get end-block proposal)))
                   (approved (is-proposal-approved proposal-id)))
               {
                 exists: true,
                 voting-active: (and (>= current-block (get start-block proposal))
                                    (< current-block (get end-block proposal))),
                 voting-ended: voting-ended,
                 approved: approved,
                 executed: (get executed proposal),
                 votes-for: (get votes-for proposal),
                 votes-against: (get votes-against proposal),
                 total-votes: (get total-votes proposal),
                 quorum-met: (>= (get total-votes proposal) (get-quorum-requirement))
               })
    { exists: false, voting-active: false, voting-ended: false, approved: false, executed: false, 
      votes-for: u0, votes-against: u0, total-votes: u0, quorum-met: false }
  )
)

;; Register as a voter
(define-public (register-voter (voting-power uint))
  (let ((current-voter (get-voter-info tx-sender)))
    (asserts! (is-none current-voter) ERR-ALREADY-VOTED)
    (asserts! (> voting-power u0) ERR-INSUFFICIENT-VOTING-POWER)
    
    ;; Register voter
    (map-set voters tx-sender { registered: true, voting-power: voting-power })
    (var-set total-voters (+ (var-get total-voters) u1))
    
    (print { event: "voter-registered", voter: tx-sender, voting-power: voting-power, timestamp: stacks-block-height })
    
    (ok true)
  )
)

;; Create a new proposal
(define-public (create-proposal 
                (title (string-utf8 200))
                (description (string-utf8 1000))
                (recipient principal)
                (amount uint)
                (voting-duration uint)
                (category (string-utf8 50)))
  (let ((proposal-id (var-get next-proposal-id))
        (current-block stacks-block-height)
        (voter-info (unwrap! (get-voter-info tx-sender) ERR-NOT-AUTHORIZED))
        (deposit-amount (var-get min-proposal-deposit)))
    
    ;; Validate inputs
    (asserts! (get registered voter-info) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= voting-duration MIN-VOTING-PERIOD) ERR-INVALID-DURATION)
    (asserts! (<= voting-duration MAX-VOTING-PERIOD) ERR-INVALID-DURATION)
    
    ;; Collect proposal deposit
    (try! (stx-transfer? deposit-amount tx-sender (as-contract tx-sender)))
    
    ;; Create proposal
    (map-set proposals proposal-id {
      proposer: tx-sender,
      title: title,
      description: description,
      recipient: recipient,
      amount: amount,
      start-block: (+ current-block u144), ;; Start voting next day
      end-block: (+ current-block u144 voting-duration),
      votes-for: u0,
      votes-against: u0,
      total-votes: u0,
      executed: false,
      deposit: deposit-amount
    })
    
    ;; Set category
    (map-set proposal-categories proposal-id category)
    
    ;; Update proposal counter
    (var-set next-proposal-id (+ proposal-id u1))
    
    ;; Emit event
    (emit-proposal-created proposal-id tx-sender)
    
    (ok proposal-id)
  )
)

;; Cast a vote on a proposal
(define-public (vote (proposal-id uint) (support bool))
  (let ((proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (voter-info (unwrap! (get-voter-info tx-sender) ERR-NOT-AUTHORIZED))
        (current-block stacks-block-height)
        (existing-vote (get-vote proposal-id tx-sender))
        (voting-power (get voting-power voter-info)))
    
    ;; Validate voting conditions
    (asserts! (get registered voter-info) ERR-NOT-AUTHORIZED)
    (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
    (asserts! (>= current-block (get start-block proposal)) ERR-VOTING-PERIOD-ENDED)
    (asserts! (< current-block (get end-block proposal)) ERR-VOTING-PERIOD-ENDED)
    
    ;; Record vote
    (map-set proposal-votes { proposal-id: proposal-id, voter: tx-sender }
             { voted: true, vote: support, power: voting-power })
    
    ;; Update proposal vote counts
    (map-set proposals proposal-id
             (merge proposal {
               votes-for: (if support 
                            (+ (get votes-for proposal) voting-power)
                            (get votes-for proposal)),
               votes-against: (if support
                               (get votes-against proposal)
                               (+ (get votes-against proposal) voting-power)),
               total-votes: (+ (get total-votes proposal) voting-power)
             }))
    
    ;; Emit event
    (print { event: "vote-cast", proposal-id: proposal-id, voter: tx-sender, vote: support, power: voting-power, timestamp: stacks-block-height })
    
    (ok true)
  )
)

;; Execute an approved proposal
(define-public (execute-proposal (proposal-id uint))
  (let ((proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND)))
    
    ;; Validate execution conditions
    (asserts! (>= stacks-block-height (get end-block proposal)) ERR-VOTING-PERIOD-ACTIVE)
    (asserts! (not (get executed proposal)) ERR-PROPOSAL-ALREADY-EXECUTED)
    (asserts! (is-proposal-approved proposal-id) ERR-PROPOSAL-NOT-APPROVED)
    
    ;; Mark as executed
    (map-set proposals proposal-id (merge proposal { executed: true }))
    
    ;; Return deposit to proposer
    (try! (as-contract (stx-transfer? (get deposit proposal) tx-sender (get proposer proposal))))
    
    ;; Emit event for external monitoring
    (print { event: "proposal-ready-for-execution", proposal-id: proposal-id, recipient: (get recipient proposal), amount: (get amount proposal), timestamp: stacks-block-height })
    (emit-proposal-executed proposal-id)
    
    (ok true)
  )
)

;; Administrative functions
(define-public (set-tax-dao-contract (contract-address principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set tax-dao-contract (some contract-address))
    (ok true)
  )
)

(define-public (set-min-proposal-deposit (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set min-proposal-deposit amount)
    (ok true)
  )
)

;; Update voter's voting power
(define-public (update-voting-power (new-power uint))
  (let ((current-voter (unwrap! (get-voter-info tx-sender) ERR-NOT-AUTHORIZED)))
    (asserts! (get registered current-voter) ERR-NOT-AUTHORIZED)
    (asserts! (> new-power u0) ERR-INSUFFICIENT-VOTING-POWER)
    
    (map-set voters tx-sender (merge current-voter { voting-power: new-power }))
    
    (print { event: "voting-power-updated", voter: tx-sender, new-power: new-power, timestamp: stacks-block-height })
    
    (ok true)
  )
)

;; Get comprehensive governance statistics
(define-read-only (get-governance-stats)
  {
    total-voters: (var-get total-voters),
    next-proposal-id: (var-get next-proposal-id),
    quorum-requirement: (get-quorum-requirement),
    approval-threshold: APPROVAL-THRESHOLD,
    min-voting-period: MIN-VOTING-PERIOD,
    max-voting-period: MAX-VOTING-PERIOD,
    min-proposal-deposit: (var-get min-proposal-deposit),
    tax-dao-contract: (var-get tax-dao-contract)
  }
)

;; Get active proposals (proposals currently in voting period)
(define-read-only (get-active-proposals (start-id uint) (end-id uint))
  (let ((current-block stacks-block-height))
    (filter is-proposal-active (generate-proposal-list start-id end-id))
  )
)

(define-private (generate-proposal-list (start uint) (end uint))
  (map get-proposal-with-id (generate-number-sequence start end))
)

(define-private (generate-number-sequence (start uint) (end uint))
  ;; Simple implementation for small ranges
  (list start (+ start u1) (+ start u2) (+ start u3) (+ start u4))
)

(define-private (get-proposal-with-id (id uint))
  { proposal-id: id, proposal-data: (get-proposal id) }
)

(define-private (is-proposal-active (proposal-info { proposal-id: uint, proposal-data: (optional { proposer: principal, title: (string-utf8 200), description: (string-utf8 1000), recipient: principal, amount: uint, start-block: uint, end-block: uint, votes-for: uint, votes-against: uint, total-votes: uint, executed: bool, deposit: uint }) }))
  (match (get proposal-data proposal-info)
    proposal (let ((current-block stacks-block-height))
               (and (>= current-block (get start-block proposal))
                    (< current-block (get end-block proposal))
                    (not (get executed proposal))))
    false
  )
)

;; Emergency proposal cancellation (only by contract owner)
(define-public (cancel-proposal (proposal-id uint))
  (let ((proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (get executed proposal)) ERR-PROPOSAL-ALREADY-EXECUTED)
    
    ;; Mark as executed to prevent further voting/execution
    (map-set proposals proposal-id (merge proposal { executed: true }))
    
    ;; Return deposit to proposer
    (try! (as-contract (stx-transfer? (get deposit proposal) tx-sender (get proposer proposal))))
    
    (print { event: "proposal-cancelled", proposal-id: proposal-id, timestamp: stacks-block-height })
    
    (ok true)
  )
)

