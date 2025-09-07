;; FluxonGrid - Zero-Knowledge Identity Verification Protocol
;; A comprehensive smart contract for dynamic reputation tessellation and skill authentication

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u1001))
(define-constant ERR_INSUFFICIENT_FUNDS (err u1002))
(define-constant ERR_SKILL_NOT_FOUND (err u1003))
(define-constant ERR_INVALID_THRESHOLD (err u1004))
(define-constant ERR_ALREADY_ACTIVATED (err u1005))
(define-constant ERR_INVALID_COORDINATES (err u1006))
(define-constant ERR_VERIFIER_NOT_CERTIFIED (err u1007))
(define-constant ERR_INVALID_REPUTATION_DATA (err u1008))
(define-constant ERR_ORACLE_TIMEOUT (err u1009))
(define-constant ERR_INSUFFICIENT_STAKE (err u1010))
(define-constant ERR_PROOF_ALREADY_DEPLOYED (err u1011))
(define-constant ERR_INVALID_SKILL_TYPE (err u1012))
(define-constant ERR_TESSELLATION_NOT_FOUND (err u1013))
(define-constant ERR_ATTESTATION_NOT_FOUND (err u1014))

;; Contract owner
(define-constant CONTRACT_OWNER tx-sender)

;; Configuration constants
(define-constant MIN_VERIFIER_STAKE u100000) ;; 100 STX minimum stake
(define-constant ORACLE_CONSENSUS_THRESHOLD u3) ;; Minimum oracle confirmations
(define-constant MAX_SKILL_RADIUS u10000) ;; Maximum skill domain radius
(define-constant REPUTATION_REWARD_MULTIPLIER u150) ;; 1.5x reward multiplier for reputation
(define-constant MAX_COMPETENCY_LEVEL u10)
(define-constant BLOCKS_PER_DAY u144) ;; Approximately 24 hours

;; Data structures
(define-map skill-profiles
    { profile-id: uint }
    {
        skill-type: (string-ascii 32),
        competency-x: int,
        competency-y: int,
        proficiency-level: uint,
        predicted-growth: uint,
        tokens-allocated: uint,
        status: (string-ascii 16),
        activation-block: uint,
        verification-deadline: uint,
        creator: principal
    }
)

(define-map flux-token-pool
    { domain-id: uint }
    {
        total-funds: uint,
        allocated-funds: uint,
        community-stake: uint,
        governance-threshold: uint
    }
)

(define-map certified-verifiers
    { verifier: principal }
    {
        certification-level: uint,
        stake-amount: uint,
        total-verifications: uint,
        success-rate: uint,
        domain-radius: uint,
        certification-expires: uint
    }
)

(define-map proof-tessellations
    { tessellation-id: uint }
    {
        competency-x: int,
        competency-y: int,
        proof-type: (string-ascii 32),
        fragments-available: uint,
        deployment-cost: uint,
        last-updated: uint,
        managed-by: principal
    }
)

(define-map reputation-attestations
    { attestation-id: uint }
    {
        profile-id: uint,
        verifier: principal,
        reputation-score: uint,
        peers-validated: uint,
        proofs-deployed: uint,
        verification-status: (string-ascii 16),
        oracle-confirmations: uint,
        reward-amount: uint
    }
)

(define-map oracle-assessments
    { oracle-id: principal, profile-id: uint }
    {
        competency-assessment: uint,
        skill-estimate: uint,
        peers-affected: uint,
        urgent-validations: (string-ascii 64),
        assessment-timestamp: uint,
        confidence-level: uint
    }
)

(define-map domain-resilience-data
    { domain-id: uint }
    {
        verification-index: uint,
        skill-density: uint,
        historical-attestations: uint,
        average-validation-time: uint,
        growth-rate: uint,
        tessellation-score: uint
    }
)

(define-map skill-oracle-consensus
    { profile-id: uint }
    { confirmation-count: uint }
)

;; Global state variables
(define-data-var profile-counter uint u0)
(define-data-var attestation-counter uint u0)
(define-data-var tessellation-counter uint u0)
(define-data-var total-flux-tokens uint u0)
(define-data-var global-reputation-level uint u0)
(define-data-var system-active bool true)

;; Authorization helper
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT_OWNER)
)

;; Validation helpers
(define-private (is-valid-coordinates (x int) (y int))
    (and 
        (<= x 90000000) 
        (>= x -90000000)
        (<= y 180000000) 
        (>= y -180000000)
    )
)

(define-private (is-certified-verifier (verifier principal))
    (match (map-get? certified-verifiers { verifier: verifier })
        verifier-data (and 
            (> (get stake-amount verifier-data) u0)
            (> (get certification-expires verifier-data) block-height)
        )
        false
    )
)

;; Oracle consensus validation
(define-private (validate-oracle-consensus (profile-id uint))
    (let ((confirmations (get-oracle-confirmations profile-id)))
        (>= confirmations ORACLE_CONSENSUS_THRESHOLD)
    )
)

(define-private (get-oracle-confirmations (profile-id uint))
    (default-to u0 (get confirmation-count (map-get? skill-oracle-consensus { profile-id: profile-id })))
)

(define-private (increment-oracle-confirmations (profile-id uint))
    (let ((current-count (get-oracle-confirmations profile-id)))
        (map-set skill-oracle-consensus 
            { profile-id: profile-id }
            { confirmation-count: (+ current-count u1) }
        )
    )
)

;; Calculate reward based on reputation
(define-private (calculate-reputation-reward (reputation-score uint) (base-amount uint))
    (/ (* base-amount reputation-score REPUTATION_REWARD_MULTIPLIER) u10000)
)

;; Admin Functions
(define-public (set-system-status (active bool))
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (var-set system-active active)
        (ok true)
    )
)

(define-public (update-global-reputation-level (level uint))
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (asserts! (<= level u5) ERR_INVALID_THRESHOLD)
        (var-set global-reputation-level level)
        (ok true)
    )
)

(define-public (add-flux-tokens (amount uint))
    (begin
        (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-flux-tokens (+ (var-get total-flux-tokens) amount))
        (ok true)
    )
)

(define-public (register-proof-tessellation (competency-x int) (competency-y int) (proof-type (string-ascii 32)) (fragments uint) (cost uint))
    (let ((tessellation-id (+ (var-get tessellation-counter) u1)))
        (begin
            (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
            (asserts! (is-valid-coordinates competency-x competency-y) ERR_INVALID_COORDINATES)
            (map-set proof-tessellations
                { tessellation-id: tessellation-id }
                {
                    competency-x: competency-x,
                    competency-y: competency-y,
                    proof-type: proof-type,
                    fragments-available: fragments,
                    deployment-cost: cost,
                    last-updated: block-height,
                    managed-by: tx-sender
                }
            )
            (var-set tessellation-counter tessellation-id)
            (ok tessellation-id)
        )
    )
)

;; Verifier Functions
(define-public (become-certified-verifier (stake-amount uint) (domain-radius uint))
    (begin
        (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
        (asserts! (>= stake-amount MIN_VERIFIER_STAKE) ERR_INSUFFICIENT_STAKE)
        (asserts! (<= domain-radius MAX_SKILL_RADIUS) ERR_INVALID_COORDINATES)
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        (map-set certified-verifiers
            { verifier: tx-sender }
            {
                certification-level: u1,
                stake-amount: stake-amount,
                total-verifications: u0,
                success-rate: u100,
                domain-radius: domain-radius,
                certification-expires: (+ block-height u52560) ;; ~1 year
            }
        )
        (ok true)
    )
)

(define-public (create-skill-profile (skill-type (string-ascii 32)) (competency-x int) (competency-y int) (proficiency uint) (predicted-growth uint))
    (let ((profile-id (+ (var-get profile-counter) u1)))
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-certified-verifier tx-sender) ERR_VERIFIER_NOT_CERTIFIED)
            (asserts! (is-valid-coordinates competency-x competency-y) ERR_INVALID_COORDINATES)
            (asserts! (<= proficiency MAX_COMPETENCY_LEVEL) ERR_INVALID_THRESHOLD)
            (map-set skill-profiles
                { profile-id: profile-id }
                {
                    skill-type: skill-type,
                    competency-x: competency-x,
                    competency-y: competency-y,
                    proficiency-level: proficiency,
                    predicted-growth: predicted-growth,
                    tokens-allocated: u0,
                    status: "created",
                    activation-block: block-height,
                    verification-deadline: (+ block-height BLOCKS_PER_DAY),
                    creator: tx-sender
                }
            )
            (var-set profile-counter profile-id)
            (ok profile-id)
        )
    )
)

(define-public (activate-skill-verification (profile-id uint) (token-allocation uint))
    (let ((profile (unwrap! (map-get? skill-profiles { profile-id: profile-id }) ERR_SKILL_NOT_FOUND)))
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-contract-owner) ERR_NOT_AUTHORIZED)
            (asserts! (is-eq (get status profile) "created") ERR_ALREADY_ACTIVATED)
            (asserts! (validate-oracle-consensus profile-id) ERR_ORACLE_TIMEOUT)
            (asserts! (<= token-allocation (var-get total-flux-tokens)) ERR_INSUFFICIENT_FUNDS)
            (map-set skill-profiles
                { profile-id: profile-id }
                (merge profile { 
                    status: "active",
                    tokens-allocated: token-allocation
                })
            )
            (var-set total-flux-tokens (- (var-get total-flux-tokens) token-allocation))
            (ok true)
        )
    )
)

(define-public (deploy-proof-fragments (profile-id uint) (tessellation-id uint) (fragments uint))
    (let (
        (profile (unwrap! (map-get? skill-profiles { profile-id: profile-id }) ERR_SKILL_NOT_FOUND))
        (tessellation (unwrap! (map-get? proof-tessellations { tessellation-id: tessellation-id }) ERR_TESSELLATION_NOT_FOUND))
    )
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-certified-verifier tx-sender) ERR_VERIFIER_NOT_CERTIFIED)
            (asserts! (is-eq (get status profile) "active") ERR_ALREADY_ACTIVATED)
            (asserts! (<= fragments (get fragments-available tessellation)) ERR_INSUFFICIENT_FUNDS)
            (map-set proof-tessellations
                { tessellation-id: tessellation-id }
                (merge tessellation { 
                    fragments-available: (- (get fragments-available tessellation) fragments),
                    last-updated: block-height
                })
            )
            (ok true)
        )
    )
)

(define-public (submit-reputation-attestation (profile-id uint) (peers-validated uint) (proofs-used uint) (reputation-score uint))
    (let ((attestation-id (+ (var-get attestation-counter) u1)))
        (begin
            (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
            (asserts! (is-certified-verifier tx-sender) ERR_VERIFIER_NOT_CERTIFIED)
            (asserts! (is-some (map-get? skill-profiles { profile-id: profile-id })) ERR_SKILL_NOT_FOUND)
            (asserts! (<= reputation-score u100) ERR_INVALID_REPUTATION_DATA)
            (map-set reputation-attestations
                { attestation-id: attestation-id }
                {
                    profile-id: profile-id,
                    verifier: tx-sender,
                    reputation-score: reputation-score,
                    peers-validated: peers-validated,
                    proofs-deployed: proofs-used,
                    verification-status: "pending",
                    oracle-confirmations: u0,
                    reward-amount: u0
                }
            )
            (var-set attestation-counter attestation-id)
            (ok attestation-id)
        )
    )
)

(define-public (submit-oracle-assessment (profile-id uint) (competency uint) (skill-estimate uint) (peers-affected uint) (urgent-validations (string-ascii 64)))
    (begin
        (asserts! (var-get system-active) ERR_NOT_AUTHORIZED)
        (asserts! (is-some (map-get? skill-profiles { profile-id: profile-id })) ERR_SKILL_NOT_FOUND)
        (asserts! (<= competency MAX_COMPETENCY_LEVEL) ERR_INVALID_THRESHOLD)
        (map-set oracle-assessments
            { oracle-id: tx-sender, profile-id: profile-id }
            {
                competency-assessment: competency,
                skill-estimate: skill-estimate,
                peers-affected: peers-affected,
                urgent-validations: urgent-validations,
                assessment-timestamp: block-height,
                confidence-level: u80
            }
        )
        (increment-oracle-confirmations profile-id)
        (ok true)
    )
)

(define-public (verify-reputation-attestation (attestation-id uint))
    (let (
        (attestation (unwrap!