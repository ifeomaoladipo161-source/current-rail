;; Current Rail - Skill-Based Tournament System
;; A blockchain gaming platform with dynamic tournament rails

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-tournament-full (err u104))
(define-constant err-already-registered (err u105))
(define-constant err-tournament-ended (err u106))

;; Data Variables
(define-data-var tournament-counter uint u0)
(define-data-var min-stake-amount uint u1000000) ;; 1 STX in micro-STX
(define-data-var platform-fee-percent uint u5) ;; 5% platform fee

;; Data Maps
(define-map tournaments
  uint
  {
    creator: principal,
    name: (string-ascii 50),
    tier: uint,
    max-players: uint,
    current-players: uint,
    entry-stake: uint,
    prize-pool: uint,
    status: (string-ascii 20),
    created-at: uint
  }
)

(define-map tournament-participants
  { tournament-id: uint, player: principal }
  {
    skill-rating: uint,
    stake-amount: uint,
    score: uint,
    registered-at: uint
  }
)

(define-map player-profiles
  principal
  {
    total-tournaments: uint,
    total-wins: uint,
    skill-rating: uint,
    total-earned: uint,
    active: bool
  }
)

(define-map player-balances
  principal
  uint
)

;; Private Functions
(define-private (calculate-prize-distribution (prize-pool uint) (position uint))
  (if (is-eq position u1)
    (/ (* prize-pool u50) u100) ;; 50% for 1st place
    (if (is-eq position u2)
      (/ (* prize-pool u30) u100) ;; 30% for 2nd place
      (/ (* prize-pool u20) u100) ;; 20% for 3rd place
    )
  )
)

;; Public Functions

;; Initialize or update player profile
(define-public (register-player (initial-skill-rating uint))
  (let ((existing-profile (map-get? player-profiles tx-sender)))
    (if (is-none existing-profile)
      (begin
        (map-set player-profiles tx-sender {
          total-tournaments: u0,
          total-wins: u0,
          skill-rating: initial-skill-rating,
          total-earned: u0,
          active: true
        })
        (ok true)
      )
      (ok false)
    )
  )
)

;; Create a new tournament
(define-public (create-tournament 
  (name (string-ascii 50))
  (tier uint)
  (max-players uint)
  (entry-stake uint))
  (let
    (
      (tournament-id (+ (var-get tournament-counter) u1))
    )
    (asserts! (>= entry-stake (var-get min-stake-amount)) err-insufficient-balance)
    (map-set tournaments tournament-id {
      creator: tx-sender,
      name: name,
      tier: tier,
      max-players: max-players,
      current-players: u0,
      entry-stake: entry-stake,
      prize-pool: u0,
      status: "open",
      created-at: block-height
    })
    (var-set tournament-counter tournament-id)
    (ok tournament-id)
  )
)

;; Join a tournament with stake
(define-public (join-tournament (tournament-id uint) (skill-rating uint))
  (let
    (
      (tournament (unwrap! (map-get? tournaments tournament-id) err-not-found))
      (existing-entry (map-get? tournament-participants { tournament-id: tournament-id, player: tx-sender }))
    )
    (asserts! (is-none existing-entry) err-already-registered)
    (asserts! (is-eq (get status tournament) "open") err-tournament-ended)
    (asserts! (< (get current-players tournament) (get max-players tournament)) err-tournament-full)
    
    ;; Transfer stake to contract
    (try! (stx-transfer? (get entry-stake tournament) tx-sender (as-contract tx-sender)))
    
    ;; Register participant
    (map-set tournament-participants 
      { tournament-id: tournament-id, player: tx-sender }
      {
        skill-rating: skill-rating,
        stake-amount: (get entry-stake tournament),
        score: u0,
        registered-at: block-height
      }
    )
    
    ;; Update tournament
    (map-set tournaments tournament-id
      (merge tournament {
        current-players: (+ (get current-players tournament) u1),
        prize-pool: (+ (get prize-pool tournament) (get entry-stake tournament))
      })
    )
    
    (ok true)
  )
)

;; Submit tournament results (owner only)
(define-public (finalize-tournament 
  (tournament-id uint)
  (winner principal)
  (second principal)
  (third principal))
  (let
    (
      (tournament (unwrap! (map-get? tournaments tournament-id) err-not-found))
      (prize-pool (get prize-pool tournament))
      (platform-fee (/ (* prize-pool (var-get platform-fee-percent)) u100))
      (distributable-pool (- prize-pool platform-fee))
    )
    (asserts! (is-eq tx-sender (get creator tournament)) err-owner-only)
    (asserts! (is-eq (get status tournament) "open") err-tournament-ended)
    
    ;; Distribute prizes
    (try! (as-contract (stx-transfer? (calculate-prize-distribution distributable-pool u1) tx-sender winner)))
    (try! (as-contract (stx-transfer? (calculate-prize-distribution distributable-pool u2) tx-sender second)))
    (try! (as-contract (stx-transfer? (calculate-prize-distribution distributable-pool u3) tx-sender third)))
    
    ;; Update tournament status
    (map-set tournaments tournament-id
      (merge tournament { status: "completed" })
    )
    
    ;; Update winner profile
    (match (map-get? player-profiles winner)
      profile (map-set player-profiles winner
        (merge profile {
          total-tournaments: (+ (get total-tournaments profile) u1),
          total-wins: (+ (get total-wins profile) u1),
          total-earned: (+ (get total-earned profile) (calculate-prize-distribution distributable-pool u1))
        })
      )
      false
    )
    
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-tournament (tournament-id uint))
  (map-get? tournaments tournament-id)
)

(define-read-only (get-player-profile (player principal))
  (map-get? player-profiles player)
)

(define-read-only (get-participant-info (tournament-id uint) (player principal))
  (map-get? tournament-participants { tournament-id: tournament-id, player: player })
)

(define-read-only (get-tournament-count)
  (ok (var-get tournament-counter))
)

;; Admin functions
(define-public (set-min-stake (new-amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-stake-amount new-amount)
    (ok true)
  )
)

(define-public (set-platform-fee (new-percent uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-percent u20) err-unauthorized) ;; Max 20% fee
    (var-set platform-fee-percent new-percent)
    (ok true)
  )
)