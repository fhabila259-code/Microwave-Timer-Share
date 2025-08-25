;; Shared kitchen appliance scheduling system

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-TIME (err u101))
(define-constant ERR-SLOT-TAKEN (err u102))
(define-constant ERR-RESERVATION-NOT-FOUND (err u103))
(define-constant ERR-PAST-TIME (err u104))

(define-data-var next-reservation-id uint u1)

(define-map reservations
  { reservation-id: uint }
  {
    user: principal,
    start-block: uint,
    end-block: uint,
    heating-instructions: (string-ascii 200),
    cleanup-reminder: (string-ascii 100)
  }
)

(define-map user-reservations
  { user: principal, block-range: uint }
  { reservation-id: uint }
)

(define-read-only (get-reservation (reservation-id uint))
  (map-get? reservations { reservation-id: reservation-id })
)

(define-read-only (get-current-reservation)
  (fold check-active-reservation
        (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
        none)
)

(define-private (check-active-reservation (id uint) (current (optional { user: principal, start-block: uint, end-block: uint, heating-instructions: (string-ascii 200), cleanup-reminder: (string-ascii 100) })))
  (match current
    some-val (some some-val)
    (match (get-reservation id)
      some-reservation
        (if (and (>= stacks-block-height (get start-block some-reservation))
                 (<= stacks-block-height (get end-block some-reservation)))
          (some some-reservation)
          none)
      none
    )
  )
)

(define-read-only (is-slot-available (start-block uint) (end-block uint))
  (and (> start-block stacks-block-height)
       (< start-block end-block)
       (get available (fold check-slot-conflict-with-params
                           (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
                           { available: true, start: start-block, end: end-block })))
)

(define-private (check-slot-conflict-with-params (id uint) (state { available: bool, start: uint, end: uint }))
  (if (not (get available state))
    state
    (match (get-reservation id)
      some-reservation
        (if (and (< (get start state) (get end-block some-reservation))
                 (> (get end state) (get start-block some-reservation)))
          { available: false, start: (get start state), end: (get end state) }
          state)
      state
    )
  )
)

(define-public (reserve-microwave (start-block uint) (duration uint) (heating-instructions (string-ascii 200)) (cleanup-reminder (string-ascii 100)))
  (let (
    (end-block (+ start-block duration))
    (reservation-id (var-get next-reservation-id))
  )
    (asserts! (> start-block stacks-block-height) ERR-PAST-TIME)
    (asserts! (> duration u0) ERR-INVALID-TIME)
    (asserts! (is-slot-available start-block end-block) ERR-SLOT-TAKEN)

    (map-set reservations
      { reservation-id: reservation-id }
      {
        user: tx-sender,
        start-block: start-block,
        end-block: end-block,
        heating-instructions: heating-instructions,
        cleanup-reminder: cleanup-reminder
      }
    )

    (map-set user-reservations
      { user: tx-sender, block-range: start-block }
      { reservation-id: reservation-id }
    )

    (var-set next-reservation-id (+ reservation-id u1))
    (ok reservation-id)
  )
)

(define-public (cancel-reservation (reservation-id uint))
  (match (get-reservation reservation-id)
    some-reservation
      (begin
        (asserts! (is-eq tx-sender (get user some-reservation)) ERR-NOT-AUTHORIZED)
        (asserts! (> (get start-block some-reservation) stacks-block-height) ERR-PAST-TIME)
        (map-delete reservations { reservation-id: reservation-id })
        (map-delete user-reservations
          { user: tx-sender, block-range: (get start-block some-reservation) })
        (ok true)
      )
    ERR-RESERVATION-NOT-FOUND
  )
)

(define-read-only (get-user-reservations (user principal))
  (fold collect-user-reservations
        (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)
        (list))
)

(define-private (collect-user-reservations (id uint) (acc (list 10 uint)))
  (match (get-reservation id)
    some-reservation
      (if (is-eq (get user some-reservation) tx-sender)
        (unwrap-panic (as-max-len? (append acc id) u10))
        acc)
    acc
  )
)
