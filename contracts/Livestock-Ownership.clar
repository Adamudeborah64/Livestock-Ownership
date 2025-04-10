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