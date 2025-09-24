;; Music Royalty Distribution - Artist and fan revenue sharing

;; Constants
(define-constant record-label tx-sender)
(define-constant err-label-only (err u500))
(define-constant err-insufficient-royalty-tokens (err u501))
(define-constant err-invalid-holder (err u502))
(define-constant err-no-streaming-revenue (err u503))
(define-constant err-royalty-transfer-failed (err u504))
(define-constant err-streaming-update-failed (err u505))
(define-constant err-token-balance-failed (err u506))
(define-constant err-invalid-token-amount (err u507))
(define-constant err-already-exists (err u508))
(define-constant err-not-found (err u509))
(define-constant err-invalid-percentage (err u510))

;; Data Variables
(define-data-var album-title (string-ascii 32) "MusicRoyalty")
(define-data-var royalty-code (string-ascii 10) "MUZROY")
(define-data-var token-precision uint u6)
(define-data-var circulating-tokens uint u0)
(define-data-var streaming-revenue-per-token uint u0)
(define-data-var total-revenue-distributed uint u0)

;; Maps
(define-map holder-tokens principal uint)
(define-map streaming-revenue-paid principal uint)
(define-map artist-collaborators principal {percentage: uint, total-earned: uint})
(define-map revenue-snapshots uint {timestamp: uint, revenue-per-token: uint, total-supply: uint})

;; Private Functions
(define-private (is-record-label)
    (is-eq tx-sender record-label))

(define-private (validate-holder (holder principal))
    (if (and (is-eq tx-sender holder) (is-some (map-get? holder-tokens holder)))
        (ok true)
        err-invalid-holder))

;; Token Management
(define-private (expand-tokens (holder principal) (tokens uint))
    (let ((expanded-balance (+ (default-to u0 (map-get? holder-tokens holder)) tokens)))
        (if (map-set holder-tokens holder expanded-balance)
            (ok true)
            err-token-balance-failed)))

(define-private (contract-tokens (holder principal) (tokens uint))
    (let ((current-balance (default-to u0 (map-get? holder-tokens holder))))
        (asserts! (>= current-balance tokens) err-insufficient-royalty-tokens)
        (if (map-set holder-tokens holder (- current-balance tokens))
            (ok true)
            err-token-balance-failed)))

;; Streaming Revenue Functions
(define-private (calculate-unpaid-streaming-revenue (holder principal))
    (let ((tokens (default-to u0 (map-get? holder-tokens holder)))
          (paid (default-to u0 (map-get? streaming-revenue-paid holder))))
        (- (* tokens (var-get streaming-revenue-per-token)) paid)))

(define-private (update-streaming-payment-record (holder principal))
    (let ((new-payment-record (* (default-to u0 (map-get? holder-tokens holder)) 
                                (var-get streaming-revenue-per-token))))
        (if (map-set streaming-revenue-paid holder new-payment-record)
            (ok true)
            err-streaming-update-failed)))

;; Public Functions
(define-public (exchange-royalty-tokens (tokens uint) 
                                       (from-holder principal) 
                                       (to-holder principal) 
                                       (exchange-note (optional (buff 34))))
    (begin
        (asserts! (> tokens u0) err-invalid-token-amount)
        (asserts! (is-some (map-get? holder-tokens from-holder)) err-invalid-holder)
        (asserts! (is-some (map-get? holder-tokens to-holder)) err-invalid-holder)
        (try! (validate-holder from-holder))
        (try! (update-streaming-payment-record from-holder))
        (try! (update-streaming-payment-record to-holder))
        (try! (contract-tokens from-holder tokens))
        (try! (expand-tokens to-holder tokens))
        (ok true)))

(define-public (drop-royalty-tokens (tokens uint) (holder principal))
    (begin
        (asserts! (is-record-label) err-label-only)
        (asserts! (> tokens u0) err-invalid-token-amount)
        (asserts! (is-some (map-get? holder-tokens holder)) err-invalid-holder)
        (try! (update-streaming-payment-record holder))
        (var-set circulating-tokens (+ (var-get circulating-tokens) tokens))
        (try! (expand-tokens holder tokens))
        (ok true)))

(define-public (release-streaming-revenue)
    (let ((revenue-pool (stx-get-balance tx-sender)))
        (begin
            (asserts! (is-record-label) err-label-only)
            (asserts! (> (var-get circulating-tokens) u0) err-royalty-transfer-failed)
            (var-set streaming-revenue-per-token 
                (+ (var-get streaming-revenue-per-token)
                   (/ (* revenue-pool u1000000) (var-get circulating-tokens))))
            (try! (stx-transfer? revenue-pool tx-sender (as-contract tx-sender)))
            (ok true))))

(define-public (collect-streaming-revenue)
    (let ((revenue-amount (calculate-unpaid-streaming-revenue tx-sender)))
        (begin
            (asserts! (> revenue-amount u0) err-no-streaming-revenue)
            (try! (update-streaming-payment-record tx-sender))
            (try! (as-contract (stx-transfer? revenue-amount tx-sender tx-sender)))
            (ok true))))

;; NEW FUNCTION 1: Register Artist Collaborators
(define-public (register-collaborator (collaborator principal) (percentage uint))
    (begin
        (asserts! (is-record-label) err-label-only)
        (asserts! (<= percentage u10000) err-invalid-percentage) ;; Max 100% (10000 basis points)
        (asserts! (> percentage u0) err-invalid-percentage)
        (asserts! (is-none (map-get? artist-collaborators collaborator)) err-already-exists)
        (map-set artist-collaborators collaborator {percentage: percentage, total-earned: u0})
        (ok true)))

;; NEW FUNCTION 2: Bulk Token Distribution
(define-public (bulk-token-drop (recipients (list 50 {holder: principal, amount: uint})))
    (begin
        (asserts! (is-record-label) err-label-only)
        (fold check-and-distribute recipients (ok u0))))

(define-private (check-and-distribute (recipient {holder: principal, amount: uint}) (previous-result (response uint uint)))
    (match previous-result
        success (begin
                    (asserts! (> (get amount recipient) u0) err-invalid-token-amount)
                    (try! (update-streaming-payment-record (get holder recipient)))
                    (var-set circulating-tokens (+ (var-get circulating-tokens) (get amount recipient)))
                    (try! (expand-tokens (get holder recipient) (get amount recipient)))
                    (ok (+ success (get amount recipient))))
        error (err error)))

;; NEW FUNCTION 3: Create Revenue Snapshot
(define-public (create-revenue-snapshot (snapshot-id uint))
    (begin
        (asserts! (is-record-label) err-label-only)
        (asserts! (is-none (map-get? revenue-snapshots snapshot-id)) err-already-exists)
        (map-set revenue-snapshots snapshot-id 
            {
                timestamp: block-height,
                revenue-per-token: (var-get streaming-revenue-per-token),
                total-supply: (var-get circulating-tokens)
            })
        (var-set total-revenue-distributed 
            (+ (var-get total-revenue-distributed) 
               (* (var-get streaming-revenue-per-token) (var-get circulating-tokens))))
        (ok true)))

;; Read-only Functions
(define-read-only (get-album-name)
    (ok (var-get album-title)))

(define-read-only (get-royalty-symbol)
    (ok (var-get royalty-code)))

(define-read-only (get-token-decimals)
    (ok (var-get token-precision)))

(define-read-only (get-holder-balance (holder principal))
    (ok (default-to u0 (map-get? holder-tokens holder))))

(define-read-only (get-circulating-supply)
    (ok (var-get circulating-tokens)))

(define-read-only (get-unpaid-streaming-revenue (holder principal))
    (ok (calculate-unpaid-streaming-revenue holder)))

;; NEW READ-ONLY FUNCTIONS for the new features
(define-read-only (get-collaborator-info (collaborator principal))
    (ok (map-get? artist-collaborators collaborator)))

(define-read-only (get-revenue-snapshot (snapshot-id uint))
    (ok (map-get? revenue-snapshots snapshot-id)))

(define-read-only (get-total-distributed-revenue)
    (ok (var-get total-revenue-distributed)))