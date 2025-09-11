(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-GOAL-EXISTS (err u102))
(define-constant ERR-GOAL-NOT-FOUND (err u103))
(define-constant ERR-GOAL-NOT-MET (err u104))
(define-constant ERR-DEADLINE-PASSED (err u105))
(define-constant ERR-SHARING-DISABLED (err u106))
(define-constant ERR-RECURRING-NOT-FOUND (err u107))
(define-constant ERR-RECURRING-LIMIT-REACHED (err u108))
(define-constant ERR-RECURRING-TOO-EARLY (err u109))

(define-data-var contract-owner principal tx-sender)

(define-map savings-goals
    { goal-id: uint }
    {
        owner: principal,
        target-amount: uint,
        current-amount: uint,
        deadline: uint,
        title: (string-ascii 50),
        created-at: uint,
        is-shareable: bool,
    }
)

(define-map user-goals
    { user: principal }
    { goals: (list 10 uint) }
)

(define-map goal-contributors
    {
        goal-id: uint,
        contributor: principal,
    }
    {
        amount-contributed: uint,
        contribution-count: uint,
    }
)

(define-map recurring-deposits
    {
        goal-id: uint,
        user: principal,
    }
    {
        deposit-amount: uint,
        interval-blocks: uint,
        last-deposit-block: uint,
        max-deposits: uint,
        deposits-made: uint,
        is-active: bool,
    }
)

(define-read-only (get-goal (goal-id uint))
    (match (map-get? savings-goals { goal-id: goal-id })
        goal (ok goal)
        (err ERR-GOAL-NOT-FOUND)
    )
)

(define-read-only (get-user-goals (user principal))
    (default-to { goals: (list) } (map-get? user-goals { user: user }))
)

(define-public (create-goal
        (title (string-ascii 50))
        (target-amount uint)
        (deadline uint)
        (is-shareable bool)
    )
    (let (
            (user-goals-data (get-user-goals tx-sender))
            (current-goals (get goals user-goals-data))
            (goal-id (+ (len current-goals) u1))
            (current-burn-block-height burn-block-height)
        )
        (asserts! (> target-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> deadline current-burn-block-height) ERR-DEADLINE-PASSED)
        (asserts! (< (len current-goals) u10) ERR-GOAL-EXISTS)
        (map-set savings-goals { goal-id: goal-id } {
            owner: tx-sender,
            target-amount: target-amount,
            current-amount: u0,
            deadline: deadline,
            title: title,
            created-at: current-burn-block-height,
            is-shareable: is-shareable,
        })
        (map-set user-goals { user: tx-sender } { goals: (unwrap-panic (as-max-len? (append current-goals goal-id) u10)) })
        (ok goal-id)
    )
)

(define-public (deposit
        (goal-id uint)
        (amount uint)
    )
    (let (
            (goal (unwrap! (map-get? savings-goals { goal-id: goal-id })
                ERR-GOAL-NOT-FOUND
            ))
            (current-amount (get current-amount goal))
            (new-amount (+ current-amount amount))
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts!
            (or
                (is-eq tx-sender (get owner goal))
                (get is-shareable goal)
            )
            ERR-SHARING-DISABLED
        )
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (let (
                (contributor-data (default-to {
                    amount-contributed: u0,
                    contribution-count: u0,
                }
                    (map-get? goal-contributors {
                        goal-id: goal-id,
                        contributor: tx-sender,
                    })
                ))
                (new-contribution-amount (+ (get amount-contributed contributor-data) amount))
                (new-contribution-count (+ (get contribution-count contributor-data) u1))
            )
            (map-set goal-contributors {
                goal-id: goal-id,
                contributor: tx-sender,
            } {
                amount-contributed: new-contribution-amount,
                contribution-count: new-contribution-count,
            })
            (map-set savings-goals { goal-id: goal-id }
                (merge goal { current-amount: new-amount })
            )
            (ok new-amount)
        )
    )
)

(define-public (withdraw (goal-id uint))
    (let (
            (goal (unwrap! (map-get? savings-goals { goal-id: goal-id })
                ERR-GOAL-NOT-FOUND
            ))
            (current-burn-block-height burn-block-height)
        )
        (asserts! (is-eq tx-sender (get owner goal)) ERR-UNAUTHORIZED)
        (asserts!
            (or
                (>= (get current-amount goal) (get target-amount goal))
                (>= current-burn-block-height (get deadline goal))
            )
            ERR-GOAL-NOT-MET
        )
        (try! (as-contract (stx-transfer? (get current-amount goal) tx-sender tx-sender)))
        (map-set savings-goals { goal-id: goal-id }
            (merge goal { current-amount: u0 })
        )
        (ok (get current-amount goal))
    )
)

(define-public (update-goal-deadline
        (goal-id uint)
        (new-deadline uint)
    )
    (let (
            (goal (unwrap! (map-get? savings-goals { goal-id: goal-id })
                ERR-GOAL-NOT-FOUND
            ))
            (current-burn-block-height burn-block-height)
        )
        (asserts! (is-eq tx-sender (get owner goal)) ERR-UNAUTHORIZED)
        (asserts! (> new-deadline current-burn-block-height) ERR-DEADLINE-PASSED)
        (map-set savings-goals { goal-id: goal-id }
            (merge goal { deadline: new-deadline })
        )
        (ok true)
    )
)

(define-read-only (get-goal-progress (goal-id uint))
    (match (map-get? savings-goals { goal-id: goal-id })
        goal (ok {
            percentage: (/ (* (get current-amount goal) u100) (get target-amount goal)),
            remaining: (- (get target-amount goal) (get current-amount goal)),
            is-complete: (>= (get current-amount goal) (get target-amount goal)),
        })
        (err ERR-GOAL-NOT-FOUND)
    )
)

(define-read-only (get-contributor-stats
        (goal-id uint)
        (contributor principal)
    )
    (default-to {
        amount-contributed: u0,
        contribution-count: u0,
    }
        (map-get? goal-contributors {
            goal-id: goal-id,
            contributor: contributor,
        })
    )
)

(define-public (toggle-goal-sharing (goal-id uint))
    (let ((goal (unwrap! (map-get? savings-goals { goal-id: goal-id }) ERR-GOAL-NOT-FOUND)))
        (asserts! (is-eq tx-sender (get owner goal)) ERR-UNAUTHORIZED)
        (map-set savings-goals { goal-id: goal-id }
            (merge goal { is-shareable: (not (get is-shareable goal)) })
        )
        (ok (not (get is-shareable goal)))
    )
)

(define-public (transfer-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
        (var-set contract-owner new-owner)
        (ok true)
    )
)

(define-public (setup-recurring-deposit
        (goal-id uint)
        (deposit-amount uint)
        (interval-blocks uint)
        (max-deposits uint)
    )
    (let (
            (goal (unwrap! (map-get? savings-goals { goal-id: goal-id })
                ERR-GOAL-NOT-FOUND
            ))
            (current-block burn-block-height)
        )
        (asserts! (is-eq tx-sender (get owner goal)) ERR-UNAUTHORIZED)
        (asserts! (> deposit-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (> interval-blocks u0) ERR-INVALID-AMOUNT)
        (asserts! (> max-deposits u0) ERR-INVALID-AMOUNT)
        (map-set recurring-deposits {
            goal-id: goal-id,
            user: tx-sender,
        } {
            deposit-amount: deposit-amount,
            interval-blocks: interval-blocks,
            last-deposit-block: current-block,
            max-deposits: max-deposits,
            deposits-made: u0,
            is-active: true,
        })
        (ok true)
    )
)

(define-public (execute-recurring-deposit (goal-id uint))
    (let (
            (recurring-data (unwrap!
                (map-get? recurring-deposits {
                    goal-id: goal-id,
                    user: tx-sender,
                })
                ERR-RECURRING-NOT-FOUND
            ))
            (goal (unwrap! (map-get? savings-goals { goal-id: goal-id })
                ERR-GOAL-NOT-FOUND
            ))
            (current-block burn-block-height)
            (next-deposit-block (+ (get last-deposit-block recurring-data)
                (get interval-blocks recurring-data)
            ))
        )
        (asserts! (get is-active recurring-data) ERR-RECURRING-NOT-FOUND)
        (asserts! (>= current-block next-deposit-block) ERR-RECURRING-TOO-EARLY)
        (asserts!
            (< (get deposits-made recurring-data)
                (get max-deposits recurring-data)
            )
            ERR-RECURRING-LIMIT-REACHED
        )
        (asserts! (is-eq tx-sender (get owner goal)) ERR-UNAUTHORIZED)
        (try! (stx-transfer? (get deposit-amount recurring-data) tx-sender
            (as-contract tx-sender)
        ))
        (let (
                (current-amount (get current-amount goal))
                (new-amount (+ current-amount (get deposit-amount recurring-data)))
                (new-deposits-made (+ (get deposits-made recurring-data) u1))
                (contributor-data (default-to {
                    amount-contributed: u0,
                    contribution-count: u0,
                }
                    (map-get? goal-contributors {
                        goal-id: goal-id,
                        contributor: tx-sender,
                    })
                ))
                (new-contribution-amount (+ (get amount-contributed contributor-data)
                    (get deposit-amount recurring-data)
                ))
                (new-contribution-count (+ (get contribution-count contributor-data) u1))
            )
            (map-set goal-contributors {
                goal-id: goal-id,
                contributor: tx-sender,
            } {
                amount-contributed: new-contribution-amount,
                contribution-count: new-contribution-count,
            })
            (map-set savings-goals { goal-id: goal-id }
                (merge goal { current-amount: new-amount })
            )
            (map-set recurring-deposits {
                goal-id: goal-id,
                user: tx-sender,
            }
                (merge recurring-data {
                    last-deposit-block: current-block,
                    deposits-made: new-deposits-made,
                    is-active: (< new-deposits-made (get max-deposits recurring-data)),
                })
            )
            (ok new-amount)
        )
    )
)

(define-public (cancel-recurring-deposit (goal-id uint))
    (let (
            (recurring-data (unwrap!
                (map-get? recurring-deposits {
                    goal-id: goal-id,
                    user: tx-sender,
                })
                ERR-RECURRING-NOT-FOUND
            ))
            (goal (unwrap! (map-get? savings-goals { goal-id: goal-id })
                ERR-GOAL-NOT-FOUND
            ))
        )
        (asserts! (is-eq tx-sender (get owner goal)) ERR-UNAUTHORIZED)
        (map-set recurring-deposits {
            goal-id: goal-id,
            user: tx-sender,
        }
            (merge recurring-data { is-active: false })
        )
        (ok true)
    )
)

(define-read-only (get-recurring-deposit
        (goal-id uint)
        (user principal)
    )
    (map-get? recurring-deposits {
        goal-id: goal-id,
        user: user,
    })
)

(define-read-only (can-execute-recurring-deposit
        (goal-id uint)
        (user principal)
    )
    (match (map-get? recurring-deposits {
        goal-id: goal-id,
        user: user,
    })
        recurring-data (let (
                (current-block burn-block-height)
                (next-deposit-block (+ (get last-deposit-block recurring-data)
                    (get interval-blocks recurring-data)
                ))
            )
            (ok {
                can-execute: (and
                    (get is-active recurring-data)
                    (>= current-block next-deposit-block)
                    (< (get deposits-made recurring-data)
                        (get max-deposits recurring-data)
                    )
                ),
                next-deposit-block: next-deposit-block,
                deposits-remaining: (- (get max-deposits recurring-data)
                    (get deposits-made recurring-data)
                ),
            })
        )
        (err ERR-RECURRING-NOT-FOUND)
    )
)
