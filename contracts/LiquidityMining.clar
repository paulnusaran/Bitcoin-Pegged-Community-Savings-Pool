;; LiquidityMining - Rewards system for long-term stakers
;; Distributes governance tokens based on stake duration and amount

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u400))
(define-constant err-no-stake (err u401))
(define-constant err-insufficient-rewards (err u402))
(define-constant err-mining-not-active (err u403))
(define-constant err-cooldown-active (err u404))

;; Mining program constants
(define-constant base-reward-rate u100) ;; 1% base APY in basis points
(define-constant duration-multiplier u50) ;; 0.5% bonus per month
(define-constant blocks-per-month u4320) ;; ~30 days
(define-constant max-duration-bonus u500) ;; Max 5% bonus
(define-constant claim-cooldown u144) ;; 24 hours

;; Program state
(define-data-var mining-active bool true)
(define-data-var total-reward-pool uint u0)
(define-data-var total-distributed uint u0)
(define-data-var program-start-block uint u0)
(define-data-var next-epoch uint u1)

;; Track mining positions
(define-map mining-positions
    principal
    {
        staked-amount: uint,
        start-block: uint,
        last-claim-block: uint,
        total-earned: uint,
        duration-tier: uint
    }
)

;; Track epoch rewards for fair distribution
(define-map epoch-rewards
    uint
    {
        total-stakers: uint,
        reward-per-staker: uint,
        epoch-start: uint,
        epoch-end: uint
    }
)

;; Track staker participation in epochs
(define-map staker-epochs
    { staker: principal, epoch: uint }
    { participated: bool, claimed: bool }
)

;; Initialize mining program
(define-public (initialize-mining (initial-rewards uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (is-eq (var-get program-start-block) u0) err-not-authorized)
        (try! (stx-transfer? initial-rewards tx-sender (as-contract tx-sender)))
        (var-set total-reward-pool initial-rewards)
        (var-set program-start-block stacks-block-height)
        (ok true)
    )
)

;; Join mining program (must have deposit in main contract)
(define-public (join-mining)
    (let (
        (user tx-sender)
        (current-position (map-get? mining-positions user))
    )
        (asserts! (var-get mining-active) err-mining-not-active)
        (asserts! (is-none current-position) err-not-authorized)
        
        ;; Create new mining position
        (map-set mining-positions user
            {
                staked-amount: u0, ;; Will be updated by sync function
                start-block: stacks-block-height,
                last-claim-block: stacks-block-height,
                total-earned: u0,
                duration-tier: u1
            }
        )
        (ok true)
    )
)

;; Update staked amount (called when user deposits/withdraws)
(define-public (sync-stake-amount (user principal) (new-amount uint))
    (let (
        (position (unwrap! (map-get? mining-positions user) err-no-stake))
        (blocks-staked (- stacks-block-height (get start-block position)))
        (new-tier (calculate-duration-tier blocks-staked))
    )
        (map-set mining-positions user
            (merge position {
                staked-amount: new-amount,
                duration-tier: new-tier
            })
        )
        (ok true)
    )
)

;; Calculate mining rewards based on duration and amount
(define-private (calculate-mining-reward (staked-amount uint) (blocks-staked uint) (duration-tier uint))
    (let (
        (base-reward (/ (* staked-amount base-reward-rate) u10000))
        (duration-bonus (/ (* base-reward (* duration-tier duration-multiplier)) u10000))
        (max-bonus (/ (* base-reward max-duration-bonus) u10000))
        (applied-bonus (if (> duration-bonus max-bonus) max-bonus duration-bonus))
        (total-annual-reward (+ base-reward applied-bonus))
        (blocks-reward (/ total-annual-reward u52560)) ;; Blocks per year
    )
        (* blocks-reward blocks-staked)
    )
)

;; Calculate tier based on staking duration
(define-private (calculate-duration-tier (blocks-staked uint))
    (let (
        (months-staked (/ blocks-staked blocks-per-month))
    )
        (if (> months-staked u12)
            u12
            (if (> months-staked u0)
                months-staked
                u1
            )
        )
    )
)

;; Claim accumulated mining rewards
(define-public (claim-mining-rewards)
    (let (
        (user tx-sender)
        (position (unwrap! (map-get? mining-positions user) err-no-stake))
        (last-claim (get last-claim-block position))
        (blocks-since-claim (- stacks-block-height last-claim))
        (staked-amount (get staked-amount position))
        (duration-tier (get duration-tier position))
        (reward-amount (calculate-mining-reward staked-amount blocks-since-claim duration-tier))
    )
        (asserts! (var-get mining-active) err-mining-not-active)
        (asserts! (> staked-amount u0) err-no-stake)
        (asserts! (>= blocks-since-claim claim-cooldown) err-cooldown-active)
        (asserts! (<= reward-amount (var-get total-reward-pool)) err-insufficient-rewards)
        
        ;; Update position
        (map-set mining-positions user
            (merge position {
                last-claim-block: stacks-block-height,
                total-earned: (+ (get total-earned position) reward-amount)
            })
        )
        
        ;; Distribute rewards
        (var-set total-reward-pool (- (var-get total-reward-pool) reward-amount))
        (var-set total-distributed (+ (var-get total-distributed) reward-amount))
        (try! (as-contract (stx-transfer? reward-amount (as-contract tx-sender) user)))
        (ok reward-amount)
    )
)

;; Start new reward epoch
(define-public (start-epoch (reward-amount uint))
    (let (
        (current-epoch (var-get next-epoch))
        (stakers-count (get-active-stakers-count))
        (reward-per-staker (if (> stakers-count u0) (/ reward-amount stakers-count) u0))
    )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (> reward-amount u0) err-insufficient-rewards)
        
        (map-set epoch-rewards current-epoch
            {
                total-stakers: stakers-count,
                reward-per-staker: reward-per-staker,
                epoch-start: stacks-block-height,
                epoch-end: (+ stacks-block-height blocks-per-month)
            }
        )
        
        (var-set next-epoch (+ current-epoch u1))
        (ok current-epoch)
    )
)

;; Claim epoch rewards
(define-public (claim-epoch-rewards (epoch-id uint))
    (let (
        (user tx-sender)
        (epoch-data (unwrap! (map-get? epoch-rewards epoch-id) err-not-authorized))
        (participation-key { staker: user, epoch: epoch-id })
        (participation (default-to { participated: false, claimed: false } 
                                  (map-get? staker-epochs participation-key)))
        (reward-amount (get reward-per-staker epoch-data))
    )
        (asserts! (> stacks-block-height (get epoch-end epoch-data)) err-cooldown-active)
        (asserts! (get participated participation) err-not-authorized)
        (asserts! (not (get claimed participation)) err-not-authorized)
        
        ;; Mark as claimed
        (map-set staker-epochs participation-key
            { participated: true, claimed: true }
        )
        
        ;; Transfer reward
        (try! (as-contract (stx-transfer? reward-amount (as-contract tx-sender) user)))
        (ok reward-amount)
    )
)

;; Add more rewards to the pool
(define-public (add-rewards (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-reward-pool (+ (var-get total-reward-pool) amount))
        (ok true)
    )
)

;; Toggle mining program
(define-public (set-mining-active (active bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (var-set mining-active active)
        (ok true)
    )
)

;; Emergency withdrawal of remaining rewards
(define-public (emergency-withdraw-rewards (amount uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (not (var-get mining-active)) err-mining-not-active)
        (try! (as-contract (stx-transfer? amount (as-contract tx-sender) contract-owner)))
        (var-set total-reward-pool (- (var-get total-reward-pool) amount))
        (ok true)
    )
)

;; Helper function to count active stakers
(define-private (get-active-stakers-count)
    u10 ;; Simplified - in practice would iterate through positions
)

;; Read-only functions
(define-read-only (get-mining-position (user principal))
    (map-get? mining-positions user)
)

(define-read-only (get-program-stats)
    {
        mining-active: (var-get mining-active),
        total-reward-pool: (var-get total-reward-pool),
        total-distributed: (var-get total-distributed),
        program-start: (var-get program-start-block),
        current-epoch: (var-get next-epoch)
    }
)

(define-read-only (preview-mining-rewards (user principal))
    (match (map-get? mining-positions user)
        position (let (
            (blocks-since-claim (- stacks-block-height (get last-claim-block position)))
            (staked-amount (get staked-amount position))
            (duration-tier (get duration-tier position))
        )
            (ok (calculate-mining-reward staked-amount blocks-since-claim duration-tier))
        )
        (err u0)
    )
)

(define-read-only (get-epoch-info (epoch-id uint))
    (map-get? epoch-rewards epoch-id)
)
