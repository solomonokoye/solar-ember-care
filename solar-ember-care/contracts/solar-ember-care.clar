;; Solar Ember Care - Decentralized Social Impact Platform
;; Core smart contract for Impact Embers and donation tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-project-complete (err u106))

;; Data Variables
(define-data-var next-project-id uint u1)
(define-data-var next-ember-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% in basis points

;; Project status enumeration
(define-constant project-status-active u1)
(define-constant project-status-funded u2)
(define-constant project-status-complete u3)
(define-constant project-status-cancelled u4)

;; Data Maps
(define-map projects
  uint
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    target-amount: uint,
    raised-amount: uint,
    creator: principal,
    status: uint,
    location: (string-utf8 100),
    created-at: uint,
    milestone-count: uint,
    completed-milestones: uint
  }
)

(define-map impact-embers
  uint
  {
    project-id: uint,
    donor: principal,
    amount: uint,
    created-at: uint,
    gps-coordinates: (string-utf8 50),
    verified: bool
  }
)

(define-map project-milestones
  {project-id: uint, milestone-id: uint}
  {
    description: (string-utf8 200),
    target-amount: uint,
    completed: bool,
    verification-data: (string-utf8 200),
    completed-at: (optional uint)
  }
)

(define-map donor-contributions
  {project-id: uint, donor: principal}
  {
    total-amount: uint,
    ember-count: uint,
    first-contribution: uint
  }
)

(define-map validators
  principal
  {
    reputation-score: uint,
    verified-projects: uint,
    tokens-earned: uint,
    is-active: bool
  }
)

;; Private Functions
(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-rate)) u10000)
)

;; Public Functions

;; Create a new solar project
(define-public (create-project 
  (title (string-utf8 100))
  (description (string-utf8 500))
  (target-amount uint)
  (location (string-utf8 100))
  (milestone-count uint))
  (let ((project-id (var-get next-project-id)))
    (asserts! (> target-amount u0) err-invalid-amount)
    (asserts! (> milestone-count u0) err-invalid-amount)
    
    (map-set projects project-id {
      title: title,
      description: description,
      target-amount: target-amount,
      raised-amount: u0,
      creator: tx-sender,
      status: project-status-active,
      location: location,
      created-at: block-height,
      milestone-count: milestone-count,
      completed-milestones: u0
    })
    
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

;; Create Impact Ember (make donation)
(define-public (create-impact-ember 
  (project-id uint) 
  (gps-coordinates (string-utf8 50)))
  (let (
    (project (unwrap! (map-get? projects project-id) err-not-found))
    (ember-id (var-get next-ember-id))
    (donation-amount (stx-get-balance tx-sender))
  )
    (asserts! (> donation-amount u0) err-insufficient-funds)
    (asserts! (is-eq (get status project) project-status-active) err-project-complete)
    
    ;; Transfer STX to contract
    (try! (stx-transfer? donation-amount tx-sender (as-contract tx-sender)))
    
    ;; Create Impact Ember NFT record
    (map-set impact-embers ember-id {
      project-id: project-id,
      donor: tx-sender,
      amount: donation-amount,
      created-at: block-height,
      gps-coordinates: gps-coordinates,
      verified: false
    })
    
    ;; Update project raised amount
    (map-set projects project-id 
      (merge project {raised-amount: (+ (get raised-amount project) donation-amount)})
    )
    
    ;; Update donor contributions
    (let ((existing-contribution (map-get? donor-contributions {project-id: project-id, donor: tx-sender})))
      (match existing-contribution
        contribution (map-set donor-contributions {project-id: project-id, donor: tx-sender}
          {
            total-amount: (+ (get total-amount contribution) donation-amount),
            ember-count: (+ (get ember-count contribution) u1),
            first-contribution: (get first-contribution contribution)
          })
        (map-set donor-contributions {project-id: project-id, donor: tx-sender}
          {
            total-amount: donation-amount,
            ember-count: u1,
            first-contribution: block-height
          })
      )
    )
    
    (var-set next-ember-id (+ ember-id u1))
    (ok ember-id)
  )
)

;; Add project milestone
(define-public (add-milestone 
  (project-id uint) 
  (milestone-id uint)
  (description (string-utf8 200))
  (target-amount uint))
  (let ((project (unwrap! (map-get? projects project-id) err-not-found)))
    (asserts! (is-eq tx-sender (get creator project)) err-unauthorized)
    (asserts! (> target-amount u0) err-invalid-amount)
    
    (map-set project-milestones {project-id: project-id, milestone-id: milestone-id} {
      description: description,
      target-amount: target-amount,
      completed: false,
      verification-data: u"",
      completed-at: none
    })
    
    (ok true)
  )
)

;; Verify milestone completion (for validators)
(define-public (verify-milestone 
  (project-id uint) 
  (milestone-id uint)
  (verification-data (string-utf8 200)))
  (let (
    (project (unwrap! (map-get? projects project-id) err-not-found))
    (milestone (unwrap! (map-get? project-milestones {project-id: project-id, milestone-id: milestone-id}) err-not-found))
    (validator (unwrap! (map-get? validators tx-sender) err-unauthorized))
  )
    (asserts! (get is-active validator) err-unauthorized)
    (asserts! (not (get completed milestone)) err-already-exists)
    
    ;; Mark milestone as completed
    (map-set project-milestones {project-id: project-id, milestone-id: milestone-id}
      (merge milestone {
        completed: true,
        verification-data: verification-data,
        completed-at: (some block-height)
      })
    )
    
    ;; Update project completed milestones
    (map-set projects project-id
      (merge project {completed-milestones: (+ (get completed-milestones project) u1)})
    )
    
    ;; Reward validator
    (map-set validators tx-sender
      (merge validator {
        verified-projects: (+ (get verified-projects validator) u1),
        tokens-earned: (+ (get tokens-earned validator) u100),
        reputation-score: (+ (get reputation-score validator) u10)
      })
    )
    
    ;; Release funds if milestone target is reached
    (let ((funds-to-release (get target-amount milestone)))
      (if (>= (get raised-amount project) funds-to-release)
        (begin
          (try! (as-contract (stx-transfer? funds-to-release tx-sender (get creator project))))
          (ok true)
        )
        (ok true)
      )
    )
  )
)

;; Register as validator
(define-public (register-validator)
  (begin
    (asserts! (is-none (map-get? validators tx-sender)) err-already-exists)
    
    (map-set validators tx-sender {
      reputation-score: u100,
      verified-projects: u0,
      tokens-earned: u0,
      is-active: true
    })
    
    (ok true)
  )
)

;; Verify Impact Ember
(define-public (verify-impact-ember (ember-id uint))
  (let (
    (ember (unwrap! (map-get? impact-embers ember-id) err-not-found))
    (validator (unwrap! (map-get? validators tx-sender) err-unauthorized))
  )
    (asserts! (get is-active validator) err-unauthorized)
    (asserts! (not (get verified ember)) err-already-exists)
    
    (map-set impact-embers ember-id
      (merge ember {verified: true})
    )
    
    ;; Reward validator
    (map-set validators tx-sender
      (merge validator {
        tokens-earned: (+ (get tokens-earned validator) u50),
        reputation-score: (+ (get reputation-score validator) u5)
      })
    )
    
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-project (project-id uint))
  (map-get? projects project-id)
)

(define-read-only (get-impact-ember (ember-id uint))
  (map-get? impact-embers ember-id)
)

(define-read-only (get-milestone (project-id uint) (milestone-id uint))
  (map-get? project-milestones {project-id: project-id, milestone-id: milestone-id})
)

(define-read-only (get-donor-contribution (project-id uint) (donor principal))
  (map-get? donor-contributions {project-id: project-id, donor: donor})
)

(define-read-only (get-validator (validator-address principal))
  (map-get? validators validator-address)
)

(define-read-only (get-next-project-id)
  (var-get next-project-id)
)

(define-read-only (get-next-ember-id)
  (var-get next-ember-id)
)

;; Admin functions

(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-contract-owner) err-owner-only)
    (asserts! (<= new-rate u1000) err-invalid-amount) ;; Max 10%
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

(define-public (withdraw-platform-fees (amount uint))
  (begin
    (asserts! (is-contract-owner) err-owner-only)
    (try! (as-contract (stx-transfer? amount tx-sender contract-owner)))
    (ok true)
  )
)