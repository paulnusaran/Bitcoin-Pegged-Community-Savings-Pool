

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

(define-read-only (get-rewards (user principal))
    (let (
        (user-deposit (unwrap! (map-get? deposits user) (err u0)))
        (pool-total (var-get total-pool-balance))
        (available-rewards (var-get total-rewards))
        (user-share (/ (* (get amount user-deposit) available-rewards) pool-total))
    )
        (ok (- user-share (get rewards-claimed user-deposit)))
    )
)

;; Admin functions
(define-public (set-pool-active (active bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set pool-active active)
        (ok true)
    )
)


(define-constant BRONZE-TIER u1000000)  ;; 1M STX
(define-constant SILVER-TIER u5000000)  ;; 5M STX
(define-constant GOLD-TIER u10000000)   ;; 10M STX

(define-private (get-tier-multiplier (amount uint))
    (if (>= amount GOLD-TIER)
        u150    ;; 1.5x rewards
        (if (>= amount SILVER-TIER)
            u125  ;; 1.25x rewards
            (if (>= amount BRONZE-TIER)
                u110  ;; 1.1x rewards
                u100
            )
        )
    )
)



(define-data-var emergency-mode bool false)

(define-public (emergency-withdraw)
    (let (
        (sender tx-sender)
        (user-deposit (unwrap! (map-get? deposits sender) ERR-NO-DEPOSIT))
        (deposit-amount (get amount user-deposit))
    )
        (asserts! (var-get emergency-mode) ERR-NOT-AUTHORIZED)
        (try! (as-contract (stx-transfer? deposit-amount (as-contract tx-sender) sender)))
        (map-delete deposits sender)
        (ok deposit-amount)
    )
)
(define-public (set-emergency-mode (active bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set emergency-mode active)
        (ok true)
    )
)


(define-constant COMPOUND-RATE u5) ;; 5% APY
(define-constant BLOCKS-PER-YEAR u52560)

(define-private (calculate-compound-interest (principal uint) (blocks uint))
    (let (
        (rate (/ (* COMPOUND-RATE blocks) BLOCKS-PER-YEAR))
        (interest (/ (* principal rate) u100))
    )
        (+ principal interest)
    )
)


(define-map staking-levels
    principal
    {
        level: uint,
        total-staked-blocks: uint
    }
)

(define-public (update-staking-level)
    (let (
        (user tx-sender)
        (current-level (default-to {level: u1, total-staked-blocks: u0} 
                       (map-get? staking-levels user)))
    )
        (map-set staking-levels user
            {
                level: (+ (get level current-level) u1),
                total-staked-blocks: (+ (get total-staked-blocks current-level) u1)
            }
        )
        (ok true)
    )
)


(define-map proposals
    uint
    {
        title: (string-ascii 50),
        votes-for: uint,
        votes-against: uint,
        active: bool
    }
)

(define-data-var proposal-count uint u0)

(define-public (create-proposal (title (string-ascii 50)))
    (let (
        (id (+ (var-get proposal-count) u1))
    )
        (asserts! (>= (get amount (unwrap! (get-deposit-info tx-sender) ERR-NO-DEPOSIT)) MIN-DEPOSIT) ERR-NOT-AUTHORIZED)
        (map-set proposals id
            {
                title: title,
                votes-for: u0,
                votes-against: u0,
                active: true
            }
        )
        (var-set proposal-count id)
        (ok id)
    )
)


(define-map time-weights
    principal
    {
        start-block: uint,
        weight-multiplier: uint
    }
)

(define-private (calculate-time-weight (blocks uint))
    (let (
        (base-multiplier u100)
        (bonus-per-block u1)
    )
        (+ base-multiplier (* blocks bonus-per-block))
    )
)


(define-map achievements
    principal
    {
        deposits-count: uint,
        total-staked: uint,
        longest-stake: uint
    }
)

(define-public (update-achievements)
    (let (
        (user tx-sender)
        (current-achievements (default-to 
            {deposits-count: u0, total-staked: u0, longest-stake: u0}
            (map-get? achievements user)))
    )
        (map-set achievements user
            {
                deposits-count: (+ (get deposits-count current-achievements) u1),
                total-staked: (+ (get total-staked current-achievements) 
                             (get amount (unwrap! (get-deposit-info user) ERR-NO-DEPOSIT))),
                longest-stake: (get locked-until (unwrap! (get-deposit-info user) ERR-NO-DEPOSIT))
            }
        )
        (ok true)
    )
)


(define-map referrals
    { referrer: principal, referee: principal }
    { bonus-claimed: bool }
)

(define-constant REFERRAL-BONUS u5)

(define-public (register-referral (referrer principal))
    (let (
        (referee tx-sender)
    )
        (asserts! (not (is-eq referee referrer)) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (get-deposit-info referee)) ERR-NOT-AUTHORIZED)
        (map-set referrals { referrer: referrer, referee: referee } { bonus-claimed: false })
        (ok true)
    )
)

(define-public (claim-referral-bonus (referee principal))
    (let (
        (referrer tx-sender)
        (referral-data (unwrap! (map-get? referrals { referrer: referrer, referee: referee }) ERR-NOT-AUTHORIZED))
        (referee-deposit (unwrap! (get-deposit-info referee) ERR-NO-DEPOSIT))
        (bonus-amount (/ (* (get amount referee-deposit) REFERRAL-BONUS) u100))
    )
        (asserts! (not (get bonus-claimed referral-data)) ERR-NOT-AUTHORIZED)
        (map-set referrals { referrer: referrer, referee: referee } { bonus-claimed: true })
        (try! (as-contract (stx-transfer? bonus-amount (as-contract tx-sender) referrer)))
        (ok bonus-amount)
    )
)


(define-map auto-compound
    principal
    { enabled: bool, last-compound: uint }
)

(define-constant COMPOUND-INTERVAL u144)

(define-public (toggle-auto-compound)
    (let (
        (sender tx-sender)
        (current-setting (default-to { enabled: false, last-compound: u0 } 
                         (map-get? auto-compound sender)))
    )
        (map-set auto-compound sender
            {
                enabled: (not (get enabled current-setting)),
                last-compound: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (execute-auto-compound)
    (let (
        (sender tx-sender)
        (compound-settings (unwrap! (map-get? auto-compound sender) ERR-NOT-AUTHORIZED))
        (user-deposit (unwrap! (get-deposit-info sender) ERR-NO-DEPOSIT))
        (claimable-rewards (unwrap-panic (get-rewards sender)))
    )
        (asserts! (get enabled compound-settings) ERR-NOT-AUTHORIZED)
        (asserts! (>= (- stacks-block-height (get last-compound compound-settings)) COMPOUND-INTERVAL) ERR-LOCK-PERIOD)
        (try! (claim-rewards))
        (try! (deposit claimable-rewards))
        (map-set auto-compound sender
            {
                enabled: true,
                last-compound: stacks-block-height
            }
        )
        (ok true)
    )
)