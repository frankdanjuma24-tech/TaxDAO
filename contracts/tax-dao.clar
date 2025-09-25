;; TaxDAO - Core Tax Collection and Management Contract
;; Provides transparent tax collection, fund management, and automated distribution

;; Constants for error handling
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u103))
(define-constant ERR-PROPOSAL-NOT-APPROVED (err u104))
(define-constant ERR-ALREADY-EXECUTED (err u105))
(define-constant ERR-INVALID-RECIPIENT (err u106))
(define-constant ERR-COLLECTION-DISABLED (err u107))
(define-constant ERR-WITHDRAWAL-LIMIT-EXCEEDED (err u108))

;; Contract owner (deployer)
(define-constant CONTRACT-OWNER tx-sender)

;; Maximum daily withdrawal limit (in microSTX)
(define-constant MAX-DAILY-WITHDRAWAL u50000000000) ;; 50,000 STX

;; Data variables for contract configuration
(define-data-var tax-collection-enabled bool true)
(define-data-var total-collected uint u0)
(define-data-var total-distributed uint u0)
(define-data-var governance-contract (optional principal) none)
(define-data-var daily-withdrawal-used uint u0)
(define-data-var last-withdrawal-day uint u0)

;; Maps for tracking individual contributions and allocations
(define-map taxpayer-contributions principal uint)
(define-map approved-proposals uint { recipient: principal, amount: uint, executed: bool, description: (string-utf8 500) })
(define-map proposal-counter uint uint)

;; Map for tracking daily withdrawal usage
(define-map daily-withdrawals uint uint)

;; Events for transparency
(define-private (emit-tax-collected (taxpayer principal) (amount uint))
  (print { event: "tax-collected", taxpayer: taxpayer, amount: amount, timestamp: stacks-block-height })
)

(define-private (emit-funds-distributed (proposal-id uint) (recipient principal) (amount uint))
  (print { event: "funds-distributed", proposal-id: proposal-id, recipient: recipient, amount: amount, timestamp: stacks-block-height })
)

(define-private (emit-proposal-approved (proposal-id uint) (amount uint))
  (print { event: "proposal-approved", proposal-id: proposal-id, amount: amount, timestamp: stacks-block-height })
)

;; Initialize proposal counter
(map-set proposal-counter u0 u0)

;; Read-only functions for transparency
(define-read-only (get-total-collected)
  (var-get total-collected)
)

(define-read-only (get-total-distributed)
  (var-get total-distributed)
)

(define-read-only (get-available-funds)
  (- (var-get total-collected) (var-get total-distributed))
)

(define-read-only (get-taxpayer-contribution (taxpayer principal))
  (default-to u0 (map-get? taxpayer-contributions taxpayer))
)

(define-read-only (get-proposal-details (proposal-id uint))
  (map-get? approved-proposals proposal-id)
)

(define-read-only (get-collection-status)
  (var-get tax-collection-enabled)
)

(define-read-only (get-contract-balance)
  (stx-get-balance (as-contract tx-sender))
)

(define-read-only (get-daily-withdrawal-limit)
  MAX-DAILY-WITHDRAWAL
)

(define-read-only (get-daily-withdrawal-used)
  (let ((current-day (/ stacks-block-height u144))) ;; Approximately 144 blocks per day
    (if (is-eq current-day (var-get last-withdrawal-day))
        (var-get daily-withdrawal-used)
        u0))
)

;; Core tax collection function
(define-public (collect-tax (amount uint))
  (let ((current-contribution (get-taxpayer-contribution tx-sender)))
    (asserts! (var-get tax-collection-enabled) ERR-COLLECTION-DISABLED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update records
    (map-set taxpayer-contributions tx-sender (+ current-contribution amount))
    (var-set total-collected (+ (var-get total-collected) amount))
    
    ;; Emit event
    (emit-tax-collected tx-sender amount)
    
    (ok true)
  )
)

;; Function to approve a proposal (called by governance contract)
(define-public (approve-proposal (proposal-id uint) (recipient principal) (amount uint) (description (string-utf8 500)))
  (let ((governance (var-get governance-contract)))
    ;; Check authorization
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER)
                  (and (is-some governance) (is-eq tx-sender (unwrap-panic governance))))
              ERR-NOT-AUTHORIZED)
    
    ;; Validate inputs
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= amount (get-available-funds)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Store proposal
    (map-set approved-proposals proposal-id {
      recipient: recipient,
      amount: amount,
      executed: false,
      description: description
    })
    
    ;; Emit event
    (emit-proposal-approved proposal-id amount)
    
    (ok true)
  )
)

;; Execute approved proposal
(define-public (execute-proposal (proposal-id uint))
  (let ((proposal-data (unwrap! (get-proposal-details proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (current-day (/ stacks-block-height u144))
        (daily-used (if (is-eq current-day (var-get last-withdrawal-day))
                       (var-get daily-withdrawal-used)
                       u0)))
    
    ;; Check if proposal exists and not executed
    (asserts! (not (get executed proposal-data)) ERR-ALREADY-EXECUTED)
    
    ;; Check daily withdrawal limit
    (asserts! (<= (+ (get amount proposal-data) daily-used) MAX-DAILY-WITHDRAWAL) ERR-WITHDRAWAL-LIMIT-EXCEEDED)
    
    ;; Check sufficient funds
    (asserts! (<= (get amount proposal-data) (get-available-funds)) ERR-INSUFFICIENT-FUNDS)
    
    ;; Transfer funds
    (try! (as-contract (stx-transfer? (get amount proposal-data) tx-sender (get recipient proposal-data))))
    
    ;; Update records
    (map-set approved-proposals proposal-id (merge proposal-data { executed: true }))
    (var-set total-distributed (+ (var-get total-distributed) (get amount proposal-data)))
    
    ;; Update daily withdrawal tracking
    (if (not (is-eq current-day (var-get last-withdrawal-day)))
        (begin
          (var-set last-withdrawal-day current-day)
          (var-set daily-withdrawal-used (get amount proposal-data)))
        (var-set daily-withdrawal-used (+ daily-used (get amount proposal-data))))
    
    ;; Emit event
    (emit-funds-distributed proposal-id (get recipient proposal-data) (get amount proposal-data))
    
    (ok true)
  )
)

;; Administrative functions
(define-public (set-governance-contract (governance-contract-addr principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set governance-contract (some governance-contract-addr))
    (ok true)
  )
)

(define-public (toggle-tax-collection)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set tax-collection-enabled (not (var-get tax-collection-enabled)))
    (ok (var-get tax-collection-enabled))
  )
)

;; Emergency withdrawal function (only for contract owner)
(define-public (emergency-withdrawal (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= amount (get-contract-balance)) ERR-INSUFFICIENT-FUNDS)
    
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    (print { event: "emergency-withdrawal", amount: amount, recipient: recipient, timestamp: stacks-block-height })
    
    (ok true)
  )
)

;; Function to get comprehensive contract statistics
(define-read-only (get-contract-stats)
  {
    total-collected: (var-get total-collected),
    total-distributed: (var-get total-distributed),
    available-funds: (get-available-funds),
    contract-balance: (get-contract-balance),
    collection-enabled: (var-get tax-collection-enabled),
    daily-withdrawal-used: (get-daily-withdrawal-used),
    daily-withdrawal-limit: MAX-DAILY-WITHDRAWAL,
    governance-contract: (var-get governance-contract)
  }
)

;; Batch tax collection for multiple contributors
(define-public (batch-collect-tax (contributors (list 20 { taxpayer: principal, amount: uint })))
  (let ((results (map process-single-contribution contributors)))
    (ok results)
  )
)

(define-private (process-single-contribution (contribution { taxpayer: principal, amount: uint }))
  (let ((taxpayer (get taxpayer contribution))
        (amount (get amount contribution))
        (current-contribution (get-taxpayer-contribution taxpayer)))
    
    (if (and (var-get tax-collection-enabled) (> amount u0))
        (begin
          (map-set taxpayer-contributions taxpayer (+ current-contribution amount))
          (var-set total-collected (+ (var-get total-collected) amount))
          (emit-tax-collected taxpayer amount)
          { taxpayer: taxpayer, success: true, amount: amount })
        { taxpayer: taxpayer, success: false, amount: u0 })
  )
)

;; Function to validate proposal execution eligibility
(define-read-only (can-execute-proposal (proposal-id uint))
  (match (get-proposal-details proposal-id)
    proposal-data (let ((current-day (/ stacks-block-height u144))
                       (daily-used (if (is-eq current-day (var-get last-withdrawal-day))
                                      (var-get daily-withdrawal-used)
                                      u0)))
                    {
                      exists: true,
                      executed: (get executed proposal-data),
                      sufficient-funds: (<= (get amount proposal-data) (get-available-funds)),
                      within-daily-limit: (<= (+ (get amount proposal-data) daily-used) MAX-DAILY-WITHDRAWAL),
                      can-execute: (and 
                        (not (get executed proposal-data))
                        (<= (get amount proposal-data) (get-available-funds))
                        (<= (+ (get amount proposal-data) daily-used) MAX-DAILY-WITHDRAWAL))
                    })
    { exists: false, executed: false, sufficient-funds: false, within-daily-limit: false, can-execute: false }
  )
)


