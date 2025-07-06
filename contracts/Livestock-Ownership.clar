(define-non-fungible-token livestock uint)

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-animal-exists (err u101))
(define-constant err-animal-not-found (err u102))
(define-constant err-not-owner (err u103))
(define-constant err-invalid-data (err u104))

(define-map animal-details
  { animal-id: uint }
  {
    breed: (string-ascii 30),
    birth-date: uint,
    owner: principal,
    last-vaccination: uint,
    vaccination-count: uint,
    price: uint,
    for-sale: bool
  }
)

(define-map vaccination-records
  { animal-id: uint, record-id: uint }
  {
    vaccine-name: (string-ascii 30),
    date: uint,
    vet: principal
  }
)

(define-data-var next-animal-id uint u1)
(define-data-var next-vaccination-id uint u1)

(define-public (register-animal 
    (breed (string-ascii 30))
    (birth-date uint)
    (price uint))
  (let ((animal-id (var-get next-animal-id)))
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (try! (nft-mint? livestock animal-id tx-sender))
    (map-set animal-details
      { animal-id: animal-id }
      {
        breed: breed,
        birth-date: birth-date,
        owner: tx-sender,
        last-vaccination: u0,
        vaccination-count: u0,
        price: price,
        for-sale: false
      }
    )
    (var-set next-animal-id (+ animal-id u1))
    (ok animal-id)
  )
)

(define-public (add-vaccination-record 
    (animal-id uint)
    (vaccine-name (string-ascii 30)))
  (let (
    (record-id (var-get next-vaccination-id))
    (animal (unwrap! (map-get? animal-details { animal-id: animal-id }) err-animal-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (map-set vaccination-records
      { animal-id: animal-id, record-id: record-id }
      {
        vaccine-name: vaccine-name,
        date: stacks-block-height,
        vet: tx-sender
      }
    )
    (map-set animal-details
      { animal-id: animal-id }
      (merge animal {
        last-vaccination: stacks-block-height,
        vaccination-count: (+ (get vaccination-count animal) u1)
      })
    )
    (var-set next-vaccination-id (+ record-id u1))
    (ok record-id)
  )
)

(define-public (set-sale-status (animal-id uint) (status bool) (new-price uint))
  (let ((animal (unwrap! (map-get? animal-details { animal-id: animal-id }) err-animal-not-found)))
    (asserts! (is-eq (get owner animal) tx-sender) err-not-owner)
    (map-set animal-details
      { animal-id: animal-id }
      (merge animal {
        for-sale: status,
        price: new-price
      })
    )
    (ok true)
  )
)

(define-public (transfer-ownership (animal-id uint))
  (let ((animal (unwrap! (map-get? animal-details { animal-id: animal-id }) err-animal-not-found)))
    (asserts! (get for-sale animal) err-not-authorized)
    (try! (stx-transfer? (get price animal) tx-sender (get owner animal)))
    (try! (nft-transfer? livestock animal-id (get owner animal) tx-sender))
    (map-set animal-details
      { animal-id: animal-id }
      (merge animal {
        owner: tx-sender,
        for-sale: false
      })
    )
    (ok true)
  )
)

(define-read-only (get-animal-details (animal-id uint))
  (ok (unwrap! (map-get? animal-details { animal-id: animal-id }) err-animal-not-found))
)

(define-read-only (get-vaccination-history (animal-id uint))
  (ok (map-get? vaccination-records { animal-id: animal-id, record-id: u0 }))
)

(define-read-only (get-owner (animal-id uint))
  (ok (get owner (unwrap! (map-get? animal-details { animal-id: animal-id }) err-animal-not-found)))
)


(define-public (add-batch-vaccination-records 
    (animal-ids (list 10 uint))
    (vaccine-name (string-ascii 30)))
  (let
    ((current-id (var-get next-vaccination-id)))
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (ok (map process-vaccination animal-ids vaccine-name))
  )
)

(define-private (process-vaccination (animal-id uint) (vaccine-name (string-ascii 30)))
  (let (
    (record-id (var-get next-vaccination-id))
    (animal (unwrap! (map-get? animal-details { animal-id: animal-id }) err-animal-not-found))
  )
    (map-set vaccination-records
      { animal-id: animal-id, record-id: record-id }
      {
        vaccine-name: vaccine-name,
        date: stacks-block-height,
        vet: tx-sender
      }
    )
    (map-set animal-details
      { animal-id: animal-id }
      (merge animal {
        last-vaccination: stacks-block-height,
        vaccination-count: (+ (get vaccination-count animal) u1)
      })
    )
    (var-set next-vaccination-id (+ record-id u1))
    (ok record-id)
  )
)


(define-map health-scores
  { animal-id: uint }
  { score: uint }
)

(define-read-only (calculate-health-score (animal-id uint))
  (let (
    (animal (unwrap! (map-get? animal-details { animal-id: animal-id }) err-animal-not-found))
    (current-height stacks-block-height)
    (vaccination-score (* (get vaccination-count animal) u10))
    (last-vaccination-score (if (> (- current-height (get last-vaccination animal)) u5000) u0 u50))
  )
    (ok (+ vaccination-score last-vaccination-score))
  )
)

(define-public (update-health-score (animal-id uint))
  (let (
    (new-score (unwrap! (calculate-health-score animal-id) err-animal-not-found))
  )
    (map-set health-scores
      { animal-id: animal-id }
      { score: new-score }
    )
    (ok new-score)
  )
)

(define-map breeding-records
  { breeding-id: uint }
  {
    sire-id: uint,
    dam-id: uint,
    breeding-date: uint,
    expected-birth-date: uint,
    breeding-method: (string-ascii 20),
    breeder: principal,
    status: (string-ascii 15)
  }
)

(define-map offspring-records
  { animal-id: uint }
  {
    sire-id: (optional uint),
    dam-id: (optional uint),
    breeding-id: (optional uint),
    generation: uint
  }
)

(define-map breeding-stats
  { animal-id: uint }
  {
    total-breedings: uint,
    successful-births: uint,
    last-breeding-date: uint
  }
)

(define-data-var next-breeding-id uint u1)

(define-constant err-invalid-breeding-pair (err u105))
(define-constant err-breeding-not-found (err u106))
(define-constant err-animal-too-young (err u107))

(define-private (update-breeding-stats (animal-id uint))
  (let (
    (current-stats (default-to { total-breedings: u0, successful-births: u0, last-breeding-date: u0 }
                               (map-get? breeding-stats { animal-id: animal-id })))
  )
    (map-set breeding-stats
      { animal-id: animal-id }
      (merge current-stats {
        total-breedings: (+ (get total-breedings current-stats) u1),
        last-breeding-date: stacks-block-height
      })
    )
    (ok true)
  )
)

(define-private (increment-successful-births (animal-id uint))
  (let (
    (current-stats (default-to { total-breedings: u0, successful-births: u0, last-breeding-date: u0 }
                               (map-get? breeding-stats { animal-id: animal-id })))
  )
    (map-set breeding-stats
      { animal-id: animal-id }
      (merge current-stats {
        successful-births: (+ (get successful-births current-stats) u1)
      })
    )
    (ok true)
  )
)

(define-read-only (get-breeding-record (breeding-id uint))
  (ok (unwrap! (map-get? breeding-records { breeding-id: breeding-id }) err-breeding-not-found))
)

(define-read-only (get-animal-lineage (animal-id uint))
  (ok (map-get? offspring-records { animal-id: animal-id }))
)

(define-read-only (get-breeding-stats (animal-id uint))
  (ok (map-get? breeding-stats { animal-id: animal-id }))
)

(define-read-only (calculate-breeding-success-rate (animal-id uint))
  (let (
    (stats (default-to { total-breedings: u0, successful-births: u0, last-breeding-date: u0 }
                       (map-get? breeding-stats { animal-id: animal-id })))
  )
    (if (is-eq (get total-breedings stats) u0)
      (ok u0)
      (ok (/ (* (get successful-births stats) u100) (get total-breedings stats)))
    )
  )
)

(define-map insurance-policies
  { policy-id: uint }
  {
    animal-id: uint,
    policy-holder: principal,
    coverage-type: (string-ascii 20),
    premium: uint,
    coverage-amount: uint,
    start-date: uint,
    end-date: uint,
    status: (string-ascii 10),
    min-health-score: uint
  }
)

(define-map insurance-claims
  { claim-id: uint }
  {
    policy-id: uint,
    animal-id: uint,
    claim-type: (string-ascii 20),
    claim-amount: uint,
    claim-date: uint,
    status: (string-ascii 15),
    auto-approved: bool
  }
)

(define-map insurance-pool
  { pool-id: uint }
  {
    total-funds: uint,
    active-policies: uint,
    total-claims-paid: uint
  }
)

(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var insurance-pool-balance uint u0)

(define-constant err-insufficient-funds (err u108))
(define-constant err-policy-not-found (err u109))
(define-constant err-claim-not-found (err u110))
(define-constant err-policy-expired (err u111))
(define-constant err-health-score-too-low (err u112))
(define-constant err-claim-already-processed (err u113))

(define-public (purchase-insurance-policy 
    (animal-id uint)
    (coverage-type (string-ascii 20))
    (premium uint)
    (coverage-amount uint)
    (duration uint)
    (min-health-score uint))
  (let (
    (policy-id (var-get next-policy-id))
    (animal (unwrap! (map-get? animal-details { animal-id: animal-id }) err-animal-not-found))
    (current-health-score (unwrap! (calculate-health-score animal-id) err-animal-not-found))
  )
    (asserts! (is-eq (get owner animal) tx-sender) err-not-owner)
    (asserts! (>= current-health-score min-health-score) err-health-score-too-low)
    (try! (stx-transfer? premium tx-sender (as-contract tx-sender)))
    (map-set insurance-policies
      { policy-id: policy-id }
      {
        animal-id: animal-id,
        policy-holder: tx-sender,
        coverage-type: coverage-type,
        premium: premium,
        coverage-amount: coverage-amount,
        start-date: stacks-block-height,
        end-date: (+ stacks-block-height duration),
        status: "active",
        min-health-score: min-health-score
      }
    )
    (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) premium))
    (var-set next-policy-id (+ policy-id u1))
    (ok policy-id)
  )
)

(define-public (file-insurance-claim 
    (policy-id uint)
    (claim-type (string-ascii 20))
    (claim-amount uint))
  (let (
    (claim-id (var-get next-claim-id))
    (policy (unwrap! (map-get? insurance-policies { policy-id: policy-id }) err-policy-not-found))
    (animal-id (get animal-id policy))
    (current-health-score (unwrap! (calculate-health-score animal-id) err-animal-not-found))
    (auto-approved (and 
      (is-eq (get status policy) "active")
      (>= stacks-block-height (get start-date policy))
      (<= stacks-block-height (get end-date policy))
      (>= current-health-score (get min-health-score policy))
      (<= claim-amount (get coverage-amount policy))))
  )
    (asserts! (is-eq (get policy-holder policy) tx-sender) err-not-owner)
    (asserts! (is-eq (get status policy) "active") err-policy-expired)
    (asserts! (<= stacks-block-height (get end-date policy)) err-policy-expired)
    (asserts! (<= claim-amount (get coverage-amount policy)) err-insufficient-funds)
    (map-set insurance-claims
      { claim-id: claim-id }
      {
        policy-id: policy-id,
        animal-id: animal-id,
        claim-type: claim-type,
        claim-amount: claim-amount,
        claim-date: stacks-block-height,
        status: (if auto-approved "approved" "pending"),
        auto-approved: auto-approved
      }
    )
    (var-set next-claim-id (+ claim-id u1))
    (if auto-approved
      (process-claim-payout claim-id)
      (ok claim-id))
  )
)

(define-private (process-claim-payout (claim-id uint))
  (let (
    (claim (unwrap! (map-get? insurance-claims { claim-id: claim-id }) err-claim-not-found))
    (policy (unwrap! (map-get? insurance-policies { policy-id: (get policy-id claim) }) err-policy-not-found))
    (payout-amount (get claim-amount claim))
  )
    (asserts! (>= (var-get insurance-pool-balance) payout-amount) err-insufficient-funds)
    (try! (as-contract (stx-transfer? payout-amount tx-sender (get policy-holder policy))))
    (var-set insurance-pool-balance (- (var-get insurance-pool-balance) payout-amount))
    (map-set insurance-claims
      { claim-id: claim-id }
      (merge claim { status: "paid" })
    )
    (ok claim-id)
  )
)

(define-public (validate-and-process-claim (claim-id uint))
  (let (
    (claim (unwrap! (map-get? insurance-claims { claim-id: claim-id }) err-claim-not-found))
    (policy (unwrap! (map-get? insurance-policies { policy-id: (get policy-id claim) }) err-policy-not-found))
    (animal-id (get animal-id claim))
    (current-health-score (unwrap! (calculate-health-score animal-id) err-animal-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (is-eq (get status claim) "pending") err-claim-already-processed)
    (asserts! (>= current-health-score (get min-health-score policy)) err-health-score-too-low)
    (map-set insurance-claims
      { claim-id: claim-id }
      (merge claim { status: "approved" })
    )
    (process-claim-payout claim-id)
  )
)

(define-read-only (get-insurance-policy (policy-id uint))
  (ok (unwrap! (map-get? insurance-policies { policy-id: policy-id }) err-policy-not-found))
)

(define-read-only (get-insurance-claim (claim-id uint))
  (ok (unwrap! (map-get? insurance-claims { claim-id: claim-id }) err-claim-not-found))
)

(define-read-only (get-insurance-pool-balance)
  (ok (var-get insurance-pool-balance))
)

(define-read-only (calculate-premium-discount (animal-id uint))
  (let (
    (health-score (unwrap! (calculate-health-score animal-id) err-animal-not-found))
    (vaccination-count (get vaccination-count (unwrap! (map-get? animal-details { animal-id: animal-id }) err-animal-not-found)))
  )
    (ok (+ (/ health-score u10) (if (> (* vaccination-count u5) u20) u20 (* vaccination-count u5))))
  )
)

