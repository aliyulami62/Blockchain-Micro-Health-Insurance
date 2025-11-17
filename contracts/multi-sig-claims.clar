(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_INVALID_THRESHOLD (err u201))
(define-constant ERR_CLAIM_NOT_ELIGIBLE (err u202))
(define-constant ERR_ALREADY_SIGNED (err u203))
(define-constant ERR_NOT_AUTHORITY (err u204))
(define-constant ERR_INVALID_CLAIM_ID (err u205))

(define-data-var contract-owner principal tx-sender)
(define-data-var approval-threshold uint u3)
(define-data-var claim-amount-threshold uint u100000000)
(define-data-var next-multi-sig-id uint u1)

(define-map authorities
  { authority: principal }
  { is-active: bool, added-block: uint, authority-name: (string-ascii 50) }
)

(define-map multi-sig-claims
  { multi-sig-id: uint }
  {
    original-claim-id: uint,
    amount: uint,
    required-approvals: uint,
    current-approvals: uint,
    status: (string-ascii 15),
    created-block: uint,
    finalized-block: (optional uint)
  }
)

(define-map claim-signatures
  { multi-sig-id: uint, authority: principal }
  { approved: bool, signature-block: uint, notes: (string-ascii 100) }
)

(define-public (designate-authority (authority principal) (name (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set authorities
      { authority: authority }
      { is-active: true, added-block: stacks-block-height, authority-name: name }
    )
    (ok true)
  )
)

(define-public (revoke-authority (authority principal))
  (let
    (
      (auth-info (unwrap! (map-get? authorities { authority: authority }) ERR_NOT_AUTHORITY))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set authorities
      { authority: authority }
      (merge auth-info { is-active: false })
    )
    (ok true)
  )
)

(define-public (create-multi-sig-claim (claim-id uint) (amount uint))
  (let
    (
      (multi-sig-id (var-get next-multi-sig-id))
      (threshold (var-get approval-threshold))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (>= amount (var-get claim-amount-threshold)) ERR_CLAIM_NOT_ELIGIBLE)
    (map-set multi-sig-claims
      { multi-sig-id: multi-sig-id }
      {
        original-claim-id: claim-id,
        amount: amount,
        required-approvals: threshold,
        current-approvals: u0,
        status: "pending",
        created-block: stacks-block-height,
        finalized-block: none
      }
    )
    (var-set next-multi-sig-id (+ multi-sig-id u1))
    (ok multi-sig-id)
  )
)

(define-public (sign-claim-approval (multi-sig-id uint) (approve bool) (notes (string-ascii 100)))
  (let
    (
      (auth-info (unwrap! (map-get? authorities { authority: tx-sender }) ERR_NOT_AUTHORITY))
      (claim-info (unwrap! (map-get? multi-sig-claims { multi-sig-id: multi-sig-id }) ERR_INVALID_CLAIM_ID))
    )
    (asserts! (get is-active auth-info) ERR_NOT_AUTHORITY)
    (asserts! (is-eq (get status claim-info) "pending") ERR_CLAIM_NOT_ELIGIBLE)
    (asserts! (is-none (map-get? claim-signatures { multi-sig-id: multi-sig-id, authority: tx-sender })) ERR_ALREADY_SIGNED)
    (map-set claim-signatures
      { multi-sig-id: multi-sig-id, authority: tx-sender }
      { approved: approve, signature-block: stacks-block-height, notes: notes }
    )
    (let
      (
        (new-approvals (if approve (+ (get current-approvals claim-info) u1) (get current-approvals claim-info)))
      )
      (map-set multi-sig-claims
        { multi-sig-id: multi-sig-id }
        (merge claim-info { current-approvals: new-approvals })
      )
      (ok new-approvals)
    )
  )
)

(define-public (finalize-multi-sig-claim (multi-sig-id uint))
  (let
    (
      (claim-info (unwrap! (map-get? multi-sig-claims { multi-sig-id: multi-sig-id }) ERR_INVALID_CLAIM_ID))
      (is-approved (>= (get current-approvals claim-info) (get required-approvals claim-info)))
    )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status claim-info) "pending") ERR_CLAIM_NOT_ELIGIBLE)
    (map-set multi-sig-claims
      { multi-sig-id: multi-sig-id }
      (merge claim-info {
        status: (if is-approved "approved" "rejected"),
        finalized-block: (some stacks-block-height)
      })
    )
    (ok is-approved)
  )
)

(define-public (update-approval-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (and (> new-threshold u0) (<= new-threshold u10)) ERR_INVALID_THRESHOLD)
    (var-set approval-threshold new-threshold)
    (ok true)
  )
)

(define-public (update-amount-threshold (new-amount uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (var-set claim-amount-threshold new-amount)
    (ok true)
  )
)

(define-read-only (get-multi-sig-claim (multi-sig-id uint))
  (map-get? multi-sig-claims { multi-sig-id: multi-sig-id })
)

(define-read-only (get-authority-info (authority principal))
  (map-get? authorities { authority: authority })
)

(define-read-only (get-claim-signature (multi-sig-id uint) (authority principal))
  (map-get? claim-signatures { multi-sig-id: multi-sig-id, authority: authority })
)

(define-read-only (get-multi-sig-settings)
  {
    approval-threshold: (var-get approval-threshold),
    claim-amount-threshold: (var-get claim-amount-threshold),
    total-multi-sig-claims: (- (var-get next-multi-sig-id) u1)
  }
)
