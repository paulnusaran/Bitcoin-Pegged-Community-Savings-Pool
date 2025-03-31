

;; Bitcoin-Pegged Community Savings Pool
;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))
(define-constant ERR-NO-DEPOSIT (err u102))
(define-constant ERR-LOCK-PERIOD (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))

;; Pool configuration
(define-constant LOCK-PERIOD u144)  ;; ~24 hours in blocks
(define-constant EARLY-EXIT-PENALTY u20)  ;; 20% penalty
(define-constant MIN-DEPOSIT u1000000)  ;; 1 STX minimum deposit

;; Data variables
(define-data-var total-pool-balance uint u0)
(define-data-var total-rewards uint u0)
(define-data-var pool-active bool true)

;; Data maps
(define-map deposits
    principal
    {
        amount: uint,
        locked-until: uint,
        rewards-claimed: uint
    }
)

;; Private functions
(define-private (calculate-penalty (amount uint))
    (/ (* amount EARLY-EXIT-PENALTY) u100)
)

(define-private (is-locked (user principal))
    (let (
        (user-deposit (unwrap! (map-get? deposits user) false))
        (lock-end (get locked-until user-deposit))
    )
        (< stacks-block-height lock-end)
    )
)

;; Public functions
(define-public (deposit (amount uint))
    (let (
        (sender tx-sender)
        (current-deposit (default-to 
            {
                amount: u0,
                locked-until: u0,
                rewards-claimed: u0
            }
            (map-get? deposits sender)))
    )
        (asserts! (var-get pool-active) ERR-NOT-AUTHORIZED)
        (asserts! (>= amount MIN-DEPOSIT) ERR-INVALID-AMOUNT)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount sender (as-contract tx-sender)))
        
        ;; Update deposit records
        (map-set deposits sender
            {
                amount: (+ (get amount current-deposit) amount),
                locked-until: (+ stacks-block-height LOCK-PERIOD),
                rewards-claimed: (get rewards-claimed current-deposit)
            }
        )
        
        ;; Update total pool balance
        (var-set total-pool-balance (+ (var-get total-pool-balance) amount))
        (ok true)
    )
)

(define-public (withdraw (amount uint))
    (let (
        (sender tx-sender)
        (user-deposit (unwrap! (map-get? deposits sender) ERR-NO-DEPOSIT))
        (deposit-amount (get amount user-deposit))
        (is-early-withdrawal (is-locked sender))
        (penalty (if is-early-withdrawal (calculate-penalty amount) u0))
        (withdrawal-amount (- amount penalty))
    )
        (asserts! (var-get pool-active) ERR-NOT-AUTHORIZED)
        (asserts! (>= deposit-amount amount) ERR-INSUFFICIENT-FUNDS)
        
        ;; Update deposit records
        (map-set deposits sender
            {
                amount: (- deposit-amount amount),
                locked-until: (get locked-until user-deposit),
                rewards-claimed: (get rewards-claimed user-deposit)
            }
        )
        
        ;; Handle penalty
        (if is-early-withdrawal
            (begin
                (var-set total-rewards (+ (var-get total-rewards) penalty))
                true
            )
            false
        )
        
        ;; Update pool balance
        (var-set total-pool-balance (- (var-get total-pool-balance) amount))
        
        ;; Transfer STX to user
        (try! (as-contract (stx-transfer? withdrawal-amount (as-contract tx-sender) sender)))
        (ok withdrawal-amount)
    )
)

(define-public (distribute-rewards (reward-amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (try! (stx-transfer? reward-amount tx-sender (as-contract tx-sender)))
        (var-set total-rewards (+ (var-get total-rewards) reward-amount))
        (ok true)
    )
)

(define-public (claim-rewards)
    (let (
        (sender tx-sender)
        (user-deposit (unwrap! (map-get? deposits sender) ERR-NO-DEPOSIT))
        (pool-total (var-get total-pool-balance))
        (available-rewards (var-get total-rewards))
        (user-share (/ (* (get amount user-deposit) available-rewards) pool-total))
        (claimable-amount (- user-share (get rewards-claimed user-deposit)))
    )
        (asserts! (> claimable-amount u0) ERR-INSUFFICIENT-FUNDS)
        
        ;; Update claimed rewards
        (map-set deposits sender
            {
                amount: (get amount user-deposit),
                locked-until: (get locked-until user-deposit),
                rewards-claimed: (+ (get rewards-claimed user-deposit) claimable-amount)
            }
        )
        
        ;; Transfer rewards
        (try! (as-contract (stx-transfer? claimable-amount (as-contract tx-sender) sender)))
        (ok claimable-amount)
    )
)

;; Read-only functions
(define-read-only (get-deposit-info (user principal))
    (map-get? deposits user)
)

(define-read-only (get-pool-stats)
    {
        total-balance: (var-get total-pool-balance),
        total-rewards: (var-get total-rewards),
        active: (var-get pool-active)
    }
)

;; Admin functions
(define-public (set-pool-active (active bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set pool-active active)
        (ok true)
    )
)