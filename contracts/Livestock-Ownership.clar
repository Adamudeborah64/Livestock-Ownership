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

;; Feed Management and Nutrition Tracking System
(define-map feed-suppliers
  { supplier-id: uint }
  {
    supplier-name: (string-ascii 50),
    supplier-address: principal,
    certification-level: uint,
    quality-score: uint,
    total-deliveries: uint,
    last-delivery-date: uint,
    verified: bool
  }
)

(define-map feed-types
  { feed-id: uint }
  {
    feed-name: (string-ascii 30),
    supplier-id: uint,
    protein-content: uint,
    carb-content: uint,
    fat-content: uint,
    fiber-content: uint,
    cost-per-kg: uint,
    expiry-date: uint,
    batch-number: (string-ascii 20)
  }
)

(define-map feed-records
  { animal-id: uint, feed-record-id: uint }
  {
    feed-id: uint,
    quantity-kg: uint,
    feeding-date: uint,
    feeding-time: (string-ascii 10),
    fed-by: principal,
    notes: (string-ascii 100)
  }
)

(define-map nutrition-profiles
  { animal-id: uint }
  {
    total-protein-intake: uint,
    total-carb-intake: uint,
    total-fat-intake: uint,
    total-fiber-intake: uint,
    total-feed-cost: uint,
    feeding-frequency: uint,
    last-feeding-date: uint,
    nutrition-score: uint
  }
)

(define-map feed-efficiency-stats
  { animal-id: uint }
  {
    total-feed-consumed: uint,
    weight-gain: uint,
    feed-conversion-ratio: uint,
    cost-per-kg-gain: uint,
    efficiency-score: uint,
    evaluation-period: uint
  }
)

(define-data-var next-supplier-id uint u1)
(define-data-var next-feed-id uint u1)
(define-data-var next-feed-record-id uint u1)

;; Error constants for feed management
(define-constant err-supplier-not-found (err u114))
(define-constant err-feed-not-found (err u115))
(define-constant err-feed-expired (err u116))
(define-constant err-supplier-not-verified (err u117))
(define-constant err-invalid-nutrition-data (err u118))
(define-constant err-feed-record-not-found (err u119))

;; Register new feed supplier
(define-public (register-feed-supplier 
    (supplier-name (string-ascii 50))
    (supplier-address principal)
    (certification-level uint))
  (let ((supplier-id (var-get next-supplier-id)))
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (>= certification-level u1) err-invalid-data)
    (asserts! (<= certification-level u5) err-invalid-data)
    (map-set feed-suppliers
      { supplier-id: supplier-id }
      {
        supplier-name: supplier-name,
        supplier-address: supplier-address,
        certification-level: certification-level,
        quality-score: u50,
        total-deliveries: u0,
        last-delivery-date: u0,
        verified: false
      }
    )
    (var-set next-supplier-id (+ supplier-id u1))
    (ok supplier-id)
  )
)

;; Verify feed supplier
(define-public (verify-feed-supplier (supplier-id uint) (verified bool))
  (let ((supplier (unwrap! (map-get? feed-suppliers { supplier-id: supplier-id }) err-supplier-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (map-set feed-suppliers
      { supplier-id: supplier-id }
      (merge supplier { verified: verified })
    )
    (ok true)
  )
)

;; Register new feed type
(define-public (register-feed-type
    (feed-name (string-ascii 30))
    (supplier-id uint)
    (protein-content uint)
    (carb-content uint)
    (fat-content uint)
    (fiber-content uint)
    (cost-per-kg uint)
    (expiry-date uint)
    (batch-number (string-ascii 20)))
  (let (
    (feed-id (var-get next-feed-id))
    (supplier (unwrap! (map-get? feed-suppliers { supplier-id: supplier-id }) err-supplier-not-found))
  )
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (asserts! (get verified supplier) err-supplier-not-verified)
    (asserts! (> expiry-date stacks-block-height) err-feed-expired)
    ;; Validate nutrition content totals don't exceed 100%
    (asserts! (<= (+ protein-content carb-content fat-content fiber-content) u100) err-invalid-nutrition-data)
    (map-set feed-types
      { feed-id: feed-id }
      {
        feed-name: feed-name,
        supplier-id: supplier-id,
        protein-content: protein-content,
        carb-content: carb-content,
        fat-content: fat-content,
        fiber-content: fiber-content,
        cost-per-kg: cost-per-kg,
        expiry-date: expiry-date,
        batch-number: batch-number
      }
    )
    (var-set next-feed-id (+ feed-id u1))
    (ok feed-id)
  )
)

;; Record feeding event
(define-public (record-feeding
    (animal-id uint)
    (feed-id uint)
    (quantity-kg uint)
    (feeding-time (string-ascii 10))
    (notes (string-ascii 100)))
  (let (
    (feed-record-id (var-get next-feed-record-id))
    (animal (unwrap! (map-get? animal-details { animal-id: animal-id }) err-animal-not-found))
    (feed (unwrap! (map-get? feed-types { feed-id: feed-id }) err-feed-not-found))
    (supplier (unwrap! (map-get? feed-suppliers { supplier-id: (get supplier-id feed) }) err-supplier-not-found))
  )
    (asserts! (is-eq (get owner animal) tx-sender) err-not-owner)
    (asserts! (get verified supplier) err-supplier-not-verified)
    (asserts! (> (get expiry-date feed) stacks-block-height) err-feed-expired)
    (asserts! (> quantity-kg u0) err-invalid-data)
    ;; Record the feeding event
    (map-set feed-records
      { animal-id: animal-id, feed-record-id: feed-record-id }
      {
        feed-id: feed-id,
        quantity-kg: quantity-kg,
        feeding-date: stacks-block-height,
        feeding-time: feeding-time,
        fed-by: tx-sender,
        notes: notes
      }
    )
    ;; Update nutrition profile
    (try! (update-nutrition-profile animal-id feed-id quantity-kg))
    ;; Update supplier delivery stats
    (try! (update-supplier-stats (get supplier-id feed)))
    (var-set next-feed-record-id (+ feed-record-id u1))
    (ok feed-record-id)
  )
)

;; Update nutrition profile after feeding
(define-private (update-nutrition-profile (animal-id uint) (feed-id uint) (quantity-kg uint))
  (let (
    (feed (unwrap! (map-get? feed-types { feed-id: feed-id }) err-feed-not-found))
    (current-profile (default-to 
      { 
        total-protein-intake: u0, total-carb-intake: u0, total-fat-intake: u0, 
        total-fiber-intake: u0, total-feed-cost: u0, feeding-frequency: u0,
        last-feeding-date: u0, nutrition-score: u0 
      }
      (map-get? nutrition-profiles { animal-id: animal-id })))
    (protein-intake (/ (* quantity-kg (get protein-content feed)) u100))
    (carb-intake (/ (* quantity-kg (get carb-content feed)) u100))
    (fat-intake (/ (* quantity-kg (get fat-content feed)) u100))
    (fiber-intake (/ (* quantity-kg (get fiber-content feed)) u100))
    (feed-cost (* quantity-kg (get cost-per-kg feed)))
  )
    (map-set nutrition-profiles
      { animal-id: animal-id }
      {
        total-protein-intake: (+ (get total-protein-intake current-profile) protein-intake),
        total-carb-intake: (+ (get total-carb-intake current-profile) carb-intake),
        total-fat-intake: (+ (get total-fat-intake current-profile) fat-intake),
        total-fiber-intake: (+ (get total-fiber-intake current-profile) fiber-intake),
        total-feed-cost: (+ (get total-feed-cost current-profile) feed-cost),
        feeding-frequency: (+ (get feeding-frequency current-profile) u1),
        last-feeding-date: stacks-block-height,
        nutrition-score: (calculate-nutrition-score-internal animal-id)
      }
    )
    (ok true)
  )
)

;; Update supplier delivery statistics
(define-private (update-supplier-stats (supplier-id uint))
  (let ((supplier (unwrap! (map-get? feed-suppliers { supplier-id: supplier-id }) err-supplier-not-found)))
    (map-set feed-suppliers
      { supplier-id: supplier-id }
      (merge supplier {
        total-deliveries: (+ (get total-deliveries supplier) u1),
        last-delivery-date: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Calculate nutrition score for an animal
(define-private (calculate-nutrition-score-internal (animal-id uint))
  (let (
    (profile (default-to 
      { 
        total-protein-intake: u0, total-carb-intake: u0, total-fat-intake: u0, 
        total-fiber-intake: u0, total-feed-cost: u0, feeding-frequency: u0,
        last-feeding-date: u0, nutrition-score: u0 
      }
      (map-get? nutrition-profiles { animal-id: animal-id })))
    (feeding-regularity (if (> (get feeding-frequency profile) u30) u25 
                           (/ (get feeding-frequency profile) u2)))
    (balanced-nutrition (if (and (> (get total-protein-intake profile) u50)
                                (> (get total-carb-intake profile) u100)
                                (> (get total-fiber-intake profile) u20)) u35 u15))
    (recent-feeding (if (< (- stacks-block-height (get last-feeding-date profile)) u144) u20 u5))
  )
    (+ feeding-regularity balanced-nutrition recent-feeding)
  )
)

;; Calculate feed efficiency ratio
(define-public (calculate-feed-efficiency (animal-id uint) (current-weight uint) (previous-weight uint) (evaluation-days uint))
  (let (
    (profile (unwrap! (map-get? nutrition-profiles { animal-id: animal-id }) err-animal-not-found))
    (animal (unwrap! (map-get? animal-details { animal-id: animal-id }) err-animal-not-found))
    (weight-gain (if (> current-weight previous-weight) (- current-weight previous-weight) u0))
    (feed-consumed (get total-protein-intake profile))
    (conversion-ratio (if (> weight-gain u0) (/ feed-consumed weight-gain) u0))
    (cost-per-kg-gain (if (> weight-gain u0) (/ (get total-feed-cost profile) weight-gain) u0))
    (efficiency-score (if (< conversion-ratio u3) u90 
                         (if (< conversion-ratio u5) u70 
                           (if (< conversion-ratio u8) u50 u20))))
  )
    (asserts! (is-eq (get owner animal) tx-sender) err-not-owner)
    (asserts! (> current-weight previous-weight) err-invalid-data)
    (map-set feed-efficiency-stats
      { animal-id: animal-id }
      {
        total-feed-consumed: feed-consumed,
        weight-gain: weight-gain,
        feed-conversion-ratio: conversion-ratio,
        cost-per-kg-gain: cost-per-kg-gain,
        efficiency-score: efficiency-score,
        evaluation-period: evaluation-days
      }
    )
    (ok efficiency-score)
  )
)

;; Update supplier quality score based on feed performance
(define-public (update-supplier-quality-score (supplier-id uint) (quality-adjustment int))
  (let ((supplier (unwrap! (map-get? feed-suppliers { supplier-id: supplier-id }) err-supplier-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (let ((new-score (if (> quality-adjustment 0)
                        (+ (get quality-score supplier) (to-uint quality-adjustment))
                        (if (>= (get quality-score supplier) (to-uint (- 0 quality-adjustment)))
                           (- (get quality-score supplier) (to-uint (- 0 quality-adjustment)))
                           u0))))
      (map-set feed-suppliers
        { supplier-id: supplier-id }
        (merge supplier { quality-score: (if (> new-score u100) u100 new-score) })
      )
      (ok new-score)
    )
  )
)

;; Read-only functions for feed management
(define-read-only (get-feed-supplier (supplier-id uint))
  (ok (unwrap! (map-get? feed-suppliers { supplier-id: supplier-id }) err-supplier-not-found))
)

(define-read-only (get-feed-type (feed-id uint))
  (ok (unwrap! (map-get? feed-types { feed-id: feed-id }) err-feed-not-found))
)

(define-read-only (get-nutrition-profile (animal-id uint))
  (ok (map-get? nutrition-profiles { animal-id: animal-id }))
)

(define-read-only (get-feed-efficiency-stats (animal-id uint))
  (ok (map-get? feed-efficiency-stats { animal-id: animal-id }))
)

(define-read-only (get-feeding-history (animal-id uint) (record-id uint))
  (ok (map-get? feed-records { animal-id: animal-id, feed-record-id: record-id }))
)

(define-read-only (calculate-nutrition-score (animal-id uint))
  (ok (calculate-nutrition-score-internal animal-id))
)

