(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_ALREADY_ENROLLED (err u101))
(define-constant ERR_NOT_ENROLLED (err u102))
(define-constant ERR_INSUFFICIENT_BALANCE (err u103))
(define-constant ERR_INVALID_AMOUNT (err u104))
(define-constant ERR_CLAIM_NOT_FOUND (err u105))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u106))
(define-constant ERR_INVALID_PROVIDER (err u107))
(define-constant ERR_PREMIUM_NOT_PAID (err u108))
(define-constant ERR_COVERAGE_EXPIRED (err u109))

(define-constant ERR_EMERGENCY_REQUEST_NOT_FOUND (err u110))
(define-constant ERR_INSUFFICIENT_EMERGENCY_FUND (err u111))
(define-constant ERR_ALREADY_VOTED (err u112))
(define-constant ERR_CANNOT_VOTE_OWN_REQUEST (err u113))
(define-constant ERR_REQUEST_ALREADY_PROCESSED (err u114))

(define-data-var emergency-fund-balance uint u0)
(define-data-var next-emergency-request-id uint u1)
(define-data-var required-votes-percentage uint u60)

(define-data-var next-member-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var monthly-premium uint u50000000)
(define-data-var max-coverage-amount uint u500000000)
(define-data-var contract-balance uint u0)

(define-map members
  { member-id: uint }
  {
    wallet: principal,
    enrollment-block: uint,
    last-premium-block: uint,
    total-premiums-paid: uint,
    is-active: bool
  }
)

(define-map member-lookup
  { wallet: principal }
  { member-id: uint }
)

(define-map verified-providers
  { provider: principal }
  { 
    name: (string-ascii 50),
    is-verified: bool,
    verification-block: uint
  }
)

(define-map claims
  { claim-id: uint }
  {
    member-id: uint,
    provider: principal,
    treatment-type: (string-ascii 100),
    amount: uint,
    submission-block: uint,
    status: (string-ascii 20),
    processed-block: (optional uint)
  }
)

(define-map member-claims
  { member-id: uint, claim-index: uint }
  { claim-id: uint }
)

(define-map member-claim-count
  { member-id: uint }
  { count: uint }
)

(define-public (enroll-member)
  (let
    (
      (member-id (var-get next-member-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-none (map-get? member-lookup { wallet: tx-sender })) ERR_ALREADY_ENROLLED)
    (try! (stx-transfer? (var-get monthly-premium) tx-sender (as-contract tx-sender)))
    (map-set members
      { member-id: member-id }
      {
        wallet: tx-sender,
        enrollment-block: current-block,
        last-premium-block: current-block,
        total-premiums-paid: (var-get monthly-premium),
        is-active: true
      }
    )
    (map-set member-lookup { wallet: tx-sender } { member-id: member-id })
    (map-set member-claim-count { member-id: member-id } { count: u0 })
    (var-set next-member-id (+ member-id u1))
    (var-set contract-balance (+ (var-get contract-balance) (var-get monthly-premium)))
    (ok member-id)
  )
)

(define-public (pay-premium)
  (let
    (
      (member-data (unwrap! (get-member-by-wallet tx-sender) ERR_NOT_ENROLLED))
      (member-id (get member-id member-data))
      (member-info (unwrap! (map-get? members { member-id: member-id }) ERR_NOT_ENROLLED))
      (current-block stacks-block-height)
    )
    (asserts! (get is-active member-info) ERR_NOT_ENROLLED)
    (try! (stx-transfer? (var-get monthly-premium) tx-sender (as-contract tx-sender)))
    (map-set members
      { member-id: member-id }
      (merge member-info {
        last-premium-block: current-block,
        total-premiums-paid: (+ (get total-premiums-paid member-info) (var-get monthly-premium))
      })
    )
    (var-set contract-balance (+ (var-get contract-balance) (var-get monthly-premium)))
    (ok true)
  )
)

(define-public (add-verified-provider (provider principal) (name (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set verified-providers
      { provider: provider }
      {
        name: name,
        is-verified: true,
        verification-block: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-public (submit-claim (provider principal) (treatment-type (string-ascii 100)) (amount uint))
  (let
    (
      (member-data (unwrap! (get-member-by-wallet tx-sender) ERR_NOT_ENROLLED))
      (member-id (get member-id member-data))
      (member-info (unwrap! (map-get? members { member-id: member-id }) ERR_NOT_ENROLLED))
      (claim-id (var-get next-claim-id))
      (current-block stacks-block-height)
      (claim-count-data (default-to { count: u0 } (map-get? member-claim-count { member-id: member-id })))
      (claim-count (get count claim-count-data))
    )
    (asserts! (get is-active member-info) ERR_NOT_ENROLLED)
    (asserts! (is-coverage-active member-info current-block) ERR_COVERAGE_EXPIRED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= amount (var-get max-coverage-amount)) ERR_INVALID_AMOUNT)
    (asserts! (is-provider-verified provider) ERR_INVALID_PROVIDER)
    (map-set claims
      { claim-id: claim-id }
      {
        member-id: member-id,
        provider: provider,
        treatment-type: treatment-type,
        amount: amount,
        submission-block: current-block,
        status: "pending",
        processed-block: none
      }
    )
    (map-set member-claims
      { member-id: member-id, claim-index: claim-count }
      { claim-id: claim-id }
    )
    (map-set member-claim-count
      { member-id: member-id }
      { count: (+ claim-count u1) }
    )
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

(define-public (approve-claim (claim-id uint))
  (let
    (
      (claim-info (unwrap! (map-get? claims { claim-id: claim-id }) ERR_CLAIM_NOT_FOUND))
      (current-block stacks-block-height)
      (claim-amount (get amount claim-info))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status claim-info) "pending") ERR_CLAIM_ALREADY_PROCESSED)
    (asserts! (>= (var-get contract-balance) claim-amount) ERR_INSUFFICIENT_BALANCE)
    (let
      (
        (member-info (unwrap! (map-get? members { member-id: (get member-id claim-info) }) ERR_NOT_ENROLLED))
        (member-wallet (get wallet member-info))
      )
      (try! (as-contract (stx-transfer? claim-amount tx-sender member-wallet)))
      (map-set claims
        { claim-id: claim-id }
        (merge claim-info {
          status: "approved",
          processed-block: (some current-block)
        })
      )
      (var-set contract-balance (- (var-get contract-balance) claim-amount))
      (ok true)
    )
  )
)

(define-public (reject-claim (claim-id uint))
  (let
    (
      (claim-info (unwrap! (map-get? claims { claim-id: claim-id }) ERR_CLAIM_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status claim-info) "pending") ERR_CLAIM_ALREADY_PROCESSED)
    (map-set claims
      { claim-id: claim-id }
      (merge claim-info {
        status: "rejected",
        processed-block: (some current-block)
      })
    )
    (ok true)
  )
)

(define-read-only (get-member-by-wallet (wallet principal))
  (map-get? member-lookup { wallet: wallet })
)

(define-read-only (get-member-info (member-id uint))
  (map-get? members { member-id: member-id })
)

(define-read-only (get-claim-info (claim-id uint))
  (map-get? claims { claim-id: claim-id })
)

(define-read-only (get-provider-info (provider principal))
  (map-get? verified-providers { provider: provider })
)

(define-read-only (is-provider-verified (provider principal))
  (match (map-get? verified-providers { provider: provider })
    provider-info (get is-verified provider-info)
    false
  )
)

(define-read-only (is-coverage-active (member-info (tuple (wallet principal) (enrollment-block uint) (last-premium-block uint) (total-premiums-paid uint) (is-active bool))) (current-block uint))
  (let
    (
      (blocks-since-payment (- current-block (get last-premium-block member-info)))
      (payment-grace-period u4320)
    )
    (and 
      (get is-active member-info)
      (<= blocks-since-payment payment-grace-period)
    )
  )
)

(define-read-only (get-member-claims (member-id uint))
  (let
    (
      (claim-count-data (default-to { count: u0 } (map-get? member-claim-count { member-id: member-id })))
      (total-claims (get count claim-count-data))
    )
    (map get-claim-by-index (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9))
  )
)

(define-read-only (get-claim-by-index (index uint))
  (let
    (
      (member-data (get-member-by-wallet tx-sender))
    )
    (match member-data
      member-info
        (let
          (
            (member-id (get member-id member-info))
            (claim-data (map-get? member-claims { member-id: member-id, claim-index: index }))
          )
          (match claim-data
            claim-info (map-get? claims { claim-id: (get claim-id claim-info) })
            none
          )
        )
      none
    )
  )
)

(define-read-only (get-contract-stats)
  {
    total-members: (- (var-get next-member-id) u1),
    total-claims: (- (var-get next-claim-id) u1),
    contract-balance: (var-get contract-balance),
    monthly-premium: (var-get monthly-premium),
    max-coverage: (var-get max-coverage-amount)
  }
)

(define-public (update-premium (new-premium uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-premium u0) ERR_INVALID_AMOUNT)
    (var-set monthly-premium new-premium)
    (ok true)
  )
)

(define-public (update-max-coverage (new-max uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-max u0) ERR_INVALID_AMOUNT)
    (var-set max-coverage-amount new-max)
    (ok true)
  )
)

(define-map emergency-fund-contributions
  { member-id: uint }
  { total-contributed: uint }
)

(define-map emergency-requests
  { request-id: uint }
  {
    requesting-member-id: uint,
    amount: uint,
    reason: (string-ascii 200),
    submission-block: uint,
    votes-for: uint,
    votes-against: uint,
    status: (string-ascii 20),
    processed-block: (optional uint)
  }
)

(define-map emergency-votes
  { request-id: uint, voter-member-id: uint }
  { vote: bool }
)

(define-public (contribute-to-emergency-fund (amount uint))
  (let
    (
      (member-data (unwrap! (get-member-by-wallet tx-sender) ERR_NOT_ENROLLED))
      (member-id (get member-id member-data))
      (current-contributions (default-to { total-contributed: u0 } 
        (map-get? emergency-fund-contributions { member-id: member-id })))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set emergency-fund-contributions
      { member-id: member-id }
      { total-contributed: (+ (get total-contributed current-contributions) amount) }
    )
    (var-set emergency-fund-balance (+ (var-get emergency-fund-balance) amount))
    (ok true)
  )
)

(define-public (request-emergency-withdrawal (amount uint) (reason (string-ascii 200)))
  (let
    (
      (member-data (unwrap! (get-member-by-wallet tx-sender) ERR_NOT_ENROLLED))
      (member-id (get member-id member-data))
      (member-info (unwrap! (map-get? members { member-id: member-id }) ERR_NOT_ENROLLED))
      (request-id (var-get next-emergency-request-id))
    )
    (asserts! (get is-active member-info) ERR_NOT_ENROLLED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (<= amount (var-get emergency-fund-balance)) ERR_INSUFFICIENT_EMERGENCY_FUND)
    (map-set emergency-requests
      { request-id: request-id }
      {
        requesting-member-id: member-id,
        amount: amount,
        reason: reason,
        submission-block: stacks-block-height,
        votes-for: u0,
        votes-against: u0,
        status: "pending",
        processed-block: none
      }
    )
    (var-set next-emergency-request-id (+ request-id u1))
    (ok request-id)
  )
)

(define-public (vote-on-emergency-request (request-id uint) (vote-for bool))
  (let
    (
      (member-data (unwrap! (get-member-by-wallet tx-sender) ERR_NOT_ENROLLED))
      (voter-member-id (get member-id member-data))
      (member-info (unwrap! (map-get? members { member-id: voter-member-id }) ERR_NOT_ENROLLED))
      (request-info (unwrap! (map-get? emergency-requests { request-id: request-id }) ERR_EMERGENCY_REQUEST_NOT_FOUND))
    )
    (asserts! (get is-active member-info) ERR_NOT_ENROLLED)
    (asserts! (not (is-eq voter-member-id (get requesting-member-id request-info))) ERR_CANNOT_VOTE_OWN_REQUEST)
    (asserts! (is-eq (get status request-info) "pending") ERR_REQUEST_ALREADY_PROCESSED)
    (asserts! (is-none (map-get? emergency-votes { request-id: request-id, voter-member-id: voter-member-id })) ERR_ALREADY_VOTED)
    (map-set emergency-votes { request-id: request-id, voter-member-id: voter-member-id } { vote: vote-for })
    (map-set emergency-requests
      { request-id: request-id }
      (merge request-info {
        votes-for: (if vote-for (+ (get votes-for request-info) u1) (get votes-for request-info)),
        votes-against: (if vote-for (get votes-against request-info) (+ (get votes-against request-info) u1))
      })
    )
    (ok true)
  )
)

(define-public (process-emergency-request (request-id uint))
  (let
    (
      (request-info (unwrap! (map-get? emergency-requests { request-id: request-id }) ERR_EMERGENCY_REQUEST_NOT_FOUND))
      (total-votes (+ (get votes-for request-info) (get votes-against request-info)))
      (approval-percentage (if (> total-votes u0) 
        (/ (* (get votes-for request-info) u100) total-votes) u0))
      (member-info (unwrap! (map-get? members { member-id: (get requesting-member-id request-info) }) ERR_NOT_ENROLLED))
      (member-wallet (get wallet member-info))
    )
    (asserts! (is-eq (get status request-info) "pending") ERR_REQUEST_ALREADY_PROCESSED)
    (asserts! (>= total-votes u3) ERR_INSUFFICIENT_BALANCE)
    (if (>= approval-percentage (var-get required-votes-percentage))
      (begin
        (try! (as-contract (stx-transfer? (get amount request-info) tx-sender member-wallet)))
        (var-set emergency-fund-balance (- (var-get emergency-fund-balance) (get amount request-info)))
        (map-set emergency-requests
          { request-id: request-id }
          (merge request-info { status: "approved", processed-block: (some stacks-block-height) })
        )
        (ok "approved")
      )
      (begin
        (map-set emergency-requests
          { request-id: request-id }
          (merge request-info { status: "rejected", processed-block: (some stacks-block-height) })
        )
        (ok "rejected")
      )
    )
  )
)

(define-read-only (get-emergency-fund-stats)
  {
    total-fund-balance: (var-get emergency-fund-balance),
    total-requests: (- (var-get next-emergency-request-id) u1),
    required-approval-percentage: (var-get required-votes-percentage)
  }
)

(define-read-only (get-emergency-request (request-id uint))
  (map-get? emergency-requests { request-id: request-id })
)
