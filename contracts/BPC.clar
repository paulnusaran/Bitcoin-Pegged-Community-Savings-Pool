

;; Bitcoin-Pegged Community Savings Pool
;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))
(define-constant ERR-NO-DEPOSIT (err u102))
(define-constant ERR-LOCK-PERIOD (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))

(define-constant BASE-INTEREST-RATE u300)
(define-constant MAX-INTEREST-RATE u1200)
(define-constant MIN-INTEREST-RATE u100)
(define-constant UTILIZATION-THRESHOLD u8000)
(define-constant RATE-ADJUSTMENT-FACTOR u50)

(define-data-var current-interest-rate uint BASE-INTEREST-RATE)
(define-data-var last-rate-update uint u0)
(define-data-var target-pool-size uint u50000000)
(define-data-var total-withdrawals uint u0)

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

(define-private (calculate-utilization-rate)
    (let (
        (current-balance (var-get total-pool-balance))
        (target-size (var-get target-pool-size))
    )
        (if (> target-size u0)
            (/ (* current-balance u10000) target-size)
            u0
        )
    )
)

(define-private (calculate-new-interest-rate)
    (let (
        (utilization (calculate-utilization-rate))
        (current-rate (var-get current-interest-rate))
    )
        (if (> utilization UTILIZATION-THRESHOLD)
            (let (
                (increase-factor (/ (* (- utilization UTILIZATION-THRESHOLD) RATE-ADJUSTMENT-FACTOR) u10000))
                (new-rate (+ current-rate increase-factor))
            )
                (if (> new-rate MAX-INTEREST-RATE)
                    MAX-INTEREST-RATE
                    new-rate
                )
            )
            (let (
                (decrease-factor (/ (* (- UTILIZATION-THRESHOLD utilization) RATE-ADJUSTMENT-FACTOR) u10000))
                (new-rate (- current-rate decrease-factor))
            )
                (if (< new-rate MIN-INTEREST-RATE)
                    MIN-INTEREST-RATE
                    new-rate
                )
            )
        )
    )
)

(define-public (update-interest-rate)
    (let (
        (new-rate (calculate-new-interest-rate))
        (blocks-since-update (- stacks-block-height (var-get last-rate-update)))
    )
        (asserts! (>= blocks-since-update u144) ERR-LOCK-PERIOD)
        (var-set current-interest-rate new-rate)
        (var-set last-rate-update stacks-block-height)
        (ok new-rate)
    )
)

(define-private (calculate-dynamic-rewards (user-amount uint) (blocks-staked uint))
    (let (
        (current-rate (var-get current-interest-rate))
        (annual-reward (/ (* user-amount current-rate) u10000))
        (block-reward (/ annual-reward BLOCKS-PER-YEAR))
        (total-reward (* block-reward blocks-staked))
    )
        total-reward
    )
)

(define-public (claim-dynamic-rewards)
    (let (
        (sender tx-sender)
        (user-deposit (unwrap! (map-get? deposits sender) ERR-NO-DEPOSIT))
        (deposit-start (get locked-until user-deposit))
        (blocks-staked (- stacks-block-height deposit-start))
        (reward-amount (calculate-dynamic-rewards (get amount user-deposit) blocks-staked))
    )
        (asserts! (> blocks-staked LOCK-PERIOD) ERR-LOCK-PERIOD)
        (asserts! (> reward-amount u0) ERR-INSUFFICIENT-FUNDS)
        
        (map-set deposits sender
            {
                amount: (get amount user-deposit),
                locked-until: (get locked-until user-deposit),
                rewards-claimed: (+ (get rewards-claimed user-deposit) reward-amount)
            }
        )
        
        (try! (as-contract (stx-transfer? reward-amount (as-contract tx-sender) sender)))
        (ok reward-amount)
    )
)

(define-public (set-target-pool-size (new-target uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> new-target u0) ERR-INVALID-AMOUNT)
        (var-set target-pool-size new-target)
        (ok true)
    )
)

(define-read-only (get-current-interest-rate)
    (var-get current-interest-rate)
)

(define-read-only (get-utilization-stats)
    {
        utilization-rate: (calculate-utilization-rate),
        current-interest-rate: (var-get current-interest-rate),
        target-pool-size: (var-get target-pool-size),
        current-pool-balance: (var-get total-pool-balance)
    }
)

(define-read-only (preview-dynamic-rewards (user principal))
    (let (
        (user-deposit (unwrap! (map-get? deposits user) (err u0)))
        (deposit-start (get locked-until user-deposit))
        (blocks-staked (- stacks-block-height deposit-start))
        (estimated-reward (calculate-dynamic-rewards (get amount user-deposit) blocks-staked))
    )
        (ok estimated-reward)
    )
)

(define-constant INSURANCE-PREMIUM-RATE u25)
(define-constant MAX-COVERAGE-RATIO u8000)
(define-constant CLAIM-PROCESSING-PERIOD u1008)
(define-constant ERR-CLAIM-EXISTS (err u200))
(define-constant ERR-INSUFFICIENT-COVERAGE (err u201))
(define-constant ERR-CLAIM-PENDING (err u202))

(define-data-var insurance-fund-balance uint u0)
(define-data-var total-coverage-amount uint u0)
(define-data-var claim-counter uint u0)

(define-map insurance-coverage
    principal
    {
        coverage-amount: uint,
        premium-paid: uint,
        active: bool
    }
)

(define-map insurance-claims
    uint
    {
        claimant: principal,
        claim-amount: uint,
        submitted-at: uint,
        status: (string-ascii 20),
        processed-at: uint
    }
)

(define-private (calculate-insurance-premium (deposit-amount uint))
    (/ (* deposit-amount INSURANCE-PREMIUM-RATE) u10000)
)

(define-private (calculate-coverage-amount (deposit-amount uint))
    (let (
        (max-coverage (/ (* deposit-amount MAX-COVERAGE-RATIO) u10000))
        (fund-balance (var-get insurance-fund-balance))
        (total-coverage (var-get total-coverage-amount))
        (available-coverage (- fund-balance total-coverage))
    )
        (if (> max-coverage available-coverage)
            available-coverage
            max-coverage
        )
    )
)

(define-public (purchase-insurance-coverage (deposit-amount uint))
    (let (
        (user tx-sender)
        (premium-amount (calculate-insurance-premium deposit-amount))
        (coverage-amount (calculate-coverage-amount deposit-amount))
        (existing-coverage (map-get? insurance-coverage user))
    )
        (asserts! (is-some (get-deposit-info user)) ERR-NO-DEPOSIT)
        (asserts! (> coverage-amount u0) ERR-INSUFFICIENT-COVERAGE)
        
        (try! (stx-transfer? premium-amount user (as-contract tx-sender)))
        
        (map-set insurance-coverage user
            {
                coverage-amount: coverage-amount,
                premium-paid: premium-amount,
                active: true
            }
        )
        
        (var-set insurance-fund-balance (+ (var-get insurance-fund-balance) premium-amount))
        (var-set total-coverage-amount (+ (var-get total-coverage-amount) coverage-amount))
        (ok coverage-amount)
    )
)

(define-public (submit-insurance-claim (claim-amount uint) (reason (string-ascii 100)))
    (let (
        (user tx-sender)
        (user-coverage (unwrap! (map-get? insurance-coverage user) ERR-NOT-AUTHORIZED))
        (claim-id (+ (var-get claim-counter) u1))
        (coverage-amount (get coverage-amount user-coverage))
    )
        (asserts! (get active user-coverage) ERR-NOT-AUTHORIZED)
        (asserts! (<= claim-amount coverage-amount) ERR-INSUFFICIENT-COVERAGE)
        
        (map-set insurance-claims claim-id
            {
                claimant: user,
                claim-amount: claim-amount,
                submitted-at: stacks-block-height,
                status: "pending",
                processed-at: u0
            }
        )
        
        (var-set claim-counter claim-id)
        (ok claim-id)
    )
)

(define-public (process-insurance-claim (claim-id uint) (approve bool))
    (let (
        (claim-data (unwrap! (map-get? insurance-claims claim-id) ERR-NOT-AUTHORIZED))
        (claimant (get claimant claim-data))
        (claim-amount (get claim-amount claim-data))
        (fund-balance (var-get insurance-fund-balance))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status claim-data) "pending") ERR-CLAIM-PENDING)
        
        (if approve
            (begin
                (asserts! (>= fund-balance claim-amount) ERR-INSUFFICIENT-FUNDS)
                (try! (as-contract (stx-transfer? claim-amount (as-contract tx-sender) claimant)))
                (var-set insurance-fund-balance (- fund-balance claim-amount))
                (map-set insurance-claims claim-id
                    {
                        claimant: claimant,
                        claim-amount: claim-amount,
                        submitted-at: (get submitted-at claim-data),
                        status: "approved",
                        processed-at: stacks-block-height
                    }
                )
                (map-set insurance-coverage claimant
                    {
                        coverage-amount: u0,
                        premium-paid: (get premium-paid (unwrap-panic (map-get? insurance-coverage claimant))),
                        active: false
                    }
                )
                (ok true)
            )
            (begin
                (map-set insurance-claims claim-id
                    {
                        claimant: claimant,
                        claim-amount: claim-amount,
                        submitted-at: (get submitted-at claim-data),
                        status: "rejected",
                        processed-at: stacks-block-height
                    }
                )
                (ok false)
            )
        )
    )
)

(define-read-only (get-insurance-coverage (user principal))
    (map-get? insurance-coverage user)
)

(define-read-only (get-insurance-fund-stats)
    {
        fund-balance: (var-get insurance-fund-balance),
        total-coverage: (var-get total-coverage-amount),
        available-coverage: (- (var-get insurance-fund-balance) (var-get total-coverage-amount))
    }
)

(define-read-only (get-claim-info (claim-id uint))
    (map-get? insurance-claims claim-id)
)

(define-read-only (preview-insurance-cost (deposit-amount uint))
    {
        premium-cost: (calculate-insurance-premium deposit-amount),
        coverage-amount: (calculate-coverage-amount deposit-amount)
    }
)