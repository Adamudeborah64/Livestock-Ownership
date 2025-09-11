;; Livestock Performance Monitoring and Analytics System
;; Tracks performance metrics, growth rates, and operational efficiency

;; Error constants
(define-constant err-not-authorized (err u200))
(define-constant err-animal-not-found (err u201))
(define-constant err-invalid-data (err u202))
(define-constant err-measurement-not-found (err u203))
(define-constant err-insufficient-data (err u204))

;; Performance measurement tracking
(define-map performance-measurements
    { animal-id: uint, measurement-id: uint }
    {
        measurement-type: (string-ascii 20), ;; "weight", "height", "length", "girth"
        value: uint,
        unit: (string-ascii 10),
        measurement-date: uint,
        measured-by: principal,
        notes: (string-ascii 100)
    }
)

;; Growth performance analytics
(define-map growth-analytics
    { animal-id: uint }
    {
        initial-weight: uint,
        current-weight: uint,
        total-weight-gain: uint,
        daily-gain-rate: uint,
        growth-efficiency-score: uint,
        last-measurement-date: uint,
        measurement-count: uint
    }
)

;; Farm-level performance metrics
(define-map farm-performance-metrics
    { farm-owner: principal, period: uint }
    {
        total-animals: uint,
        average-growth-rate: uint,
        top-performers: uint,
        underperformers: uint,
        total-feed-cost: uint,
        revenue-per-animal: uint,
        profit-margin: uint,
        operational-efficiency: uint
    }
)

;; Performance alerts and recommendations
(define-map performance-alerts
    { animal-id: uint, alert-type: (string-ascii 20) }
    {
        alert-message: (string-ascii 200),
        severity: (string-ascii 10), ;; "low", "medium", "high"
        triggered-date: uint,
        resolved: bool,
        recommended_action: (string-ascii 150)
    }
)

;; Comparative performance benchmarks
(define-map breed-benchmarks
    { breed: (string-ascii 30), age-category: (string-ascii 15) }
    {
        avg-weight: uint,
        avg-daily-gain: uint,
        optimal-feed-ratio: uint,
        benchmark-health-score: uint,
        last-updated: uint
    }
)

;; Performance trends and forecasting
(define-map performance-trends
    { animal-id: uint }
    {
        trend-direction: (string-ascii 10), ;; "improving", "stable", "declining"
        projected-weight: uint,
        projected-date: uint,
        confidence-level: uint,
        recommendation: (string-ascii 100)
    }
)

;; Data variables
(define-data-var next-measurement-id uint u1)
(define-data-var benchmark-update-frequency uint u1440) ;; blocks

;; Record performance measurement
(define-public (record-measurement 
    (animal-id uint)
    (measurement-type (string-ascii 20))
    (value uint)
    (unit (string-ascii 10))
    (notes (string-ascii 100)))
    (let
        ((measurement-id (var-get next-measurement-id)))
        
        (asserts! (> value u0) err-invalid-data)
        
        (map-set performance-measurements
            { animal-id: animal-id, measurement-id: measurement-id }
            {
                measurement-type: measurement-type,
                value: value,
                unit: unit,
                measurement-date: stacks-block-height,
                measured-by: tx-sender,
                notes: notes
            }
        )
        
        ;; Update growth analytics if weight measurement
        (if (is-eq measurement-type "weight")
            (unwrap! (update-growth-analytics animal-id value) err-invalid-data)
            true
        )
        
        ;; Check for performance alerts
        (unwrap! (check-performance-alerts animal-id value measurement-type) err-invalid-data)
        
        (var-set next-measurement-id (+ measurement-id u1))
        (ok measurement-id)
    )
)

;; Update growth analytics
(define-private (update-growth-analytics (animal-id uint) (current-weight uint))
    (let
        ((current-analytics (default-to
            {
                initial-weight: current-weight,
                current-weight: u0,
                total-weight-gain: u0,
                daily-gain-rate: u0,
                growth-efficiency-score: u0,
                last-measurement-date: stacks-block-height,
                measurement-count: u0
            }
            (map-get? growth-analytics { animal-id: animal-id })))
         (days-since-last (if (> (get last-measurement-date current-analytics) u0)
                            (- stacks-block-height (get last-measurement-date current-analytics))
                            u1))
         (weight-gain (if (> current-weight (get current-weight current-analytics))
                        (- current-weight (get current-weight current-analytics))
                        u0))
         (daily-gain (if (> days-since-last u0) (/ weight-gain days-since-last) u0))
         (new-measurement-count (+ (get measurement-count current-analytics) u1))
         (efficiency-score (calculate-efficiency-score current-weight daily-gain new-measurement-count)))
        
        (map-set growth-analytics
            { animal-id: animal-id }
            {
                initial-weight: (get initial-weight current-analytics),
                current-weight: current-weight,
                total-weight-gain: (+ (get total-weight-gain current-analytics) weight-gain),
                daily-gain-rate: daily-gain,
                growth-efficiency-score: efficiency-score,
                last-measurement-date: stacks-block-height,
                measurement-count: new-measurement-count
            }
        )
        (ok true)
    )
)

;; Calculate growth efficiency score
(define-private (calculate-efficiency-score (weight uint) (daily-gain uint) (measurements uint))
    (let
        ((weight-score (if (> weight u50) u30 (/ (* weight u30) u50)))
         (gain-score (if (> daily-gain u2) u40 (* daily-gain u20)))
         (consistency-score (if (> measurements u10) u30 (* measurements u3))))
        (+ weight-score gain-score consistency-score)
    )
)

;; Check and create performance alerts
(define-private (check-performance-alerts (animal-id uint) (value uint) (measurement-type (string-ascii 20)))
    (let
        ((analytics (map-get? growth-analytics { animal-id: animal-id })))
        
        ;; Alert for poor growth rate
        (if (and (is-some analytics) (is-eq measurement-type "weight"))
            (let ((daily-gain (get daily-gain-rate (unwrap-panic analytics))))
                (if (< daily-gain u1)
                    (map-set performance-alerts
                        { animal-id: animal-id, alert-type: "poor-growth" }
                        {
                            alert-message: "Animal showing poor growth rate - consider nutrition review",
                            severity: "medium",
                            triggered-date: stacks-block-height,
                            resolved: false,
                            recommended_action: "Review feeding schedule and consult veterinarian"
                        }
                    )
                    true
                )
            )
            true
        )
        (ok true)
    )
)

;; Generate performance report for animal
(define-public (generate-performance-report (animal-id uint))
    (let
        ((analytics (map-get? growth-analytics { animal-id: animal-id }))
         (trend (map-get? performance-trends { animal-id: animal-id })))
        
        (asserts! (is-some analytics) err-insufficient-data)
        
        (ok {
            animal-id: animal-id,
            growth-data: analytics,
            performance-trend: trend,
            efficiency-rating: (get-efficiency-rating (get growth-efficiency-score (unwrap-panic analytics))),
            improvement-suggestions: (generate-improvement-suggestions animal-id)
        })
    )
)

;; Get efficiency rating
(define-private (get-efficiency-rating (score uint))
    (if (>= score u80) "excellent"
        (if (>= score u60) "good"
            (if (>= score u40) "average" "needs-improvement")
        )
    )
)

;; Generate improvement suggestions
(define-private (generate-improvement-suggestions (animal-id uint))
    (let
        ((analytics (map-get? growth-analytics { animal-id: animal-id })))
        
        (if (is-some analytics)
            (let ((score (get growth-efficiency-score (unwrap-panic analytics))))
                (if (< score u40)
                    "Consider optimizing feed quality and increasing feeding frequency"
                    (if (< score u70)
                        "Monitor closely and maintain current feeding regimen"
                        "Excellent performance - consider as breeding candidate"
                    )
                )
            )
            "Insufficient data for recommendations"
        )
    )
)

;; Calculate farm-level performance metrics
(define-public (calculate-farm-metrics (farm-owner principal) (animal-ids (list 10 uint)))
    (let
        ((period (/ stacks-block-height u1440))
         (animal-count (len animal-ids))
         (metrics (fold calculate-animal-contribution animal-ids 
                       { total-weight: u0, total-gain: u0, high-performers: u0, low-performers: u0 })))
        
        (asserts! (is-eq tx-sender farm-owner) err-not-authorized)
        
        (map-set farm-performance-metrics
            { farm-owner: farm-owner, period: period }
            {
                total-animals: animal-count,
                average-growth-rate: (if (> animal-count u0) (/ (get total-gain metrics) animal-count) u0),
                top-performers: (get high-performers metrics),
                underperformers: (get low-performers metrics),
                total-feed-cost: u0, ;; Would integrate with feed management
                revenue-per-animal: u0, ;; Would integrate with sales data
                profit-margin: u0,
                operational-efficiency: (calculate-operational-efficiency animal-count metrics)
            }
        )
        (ok period)
    )
)

;; Helper function for farm metrics calculation
(define-private (calculate-animal-contribution (animal-id uint) 
    (acc { total-weight: uint, total-gain: uint, high-performers: uint, low-performers: uint }))
    (let
        ((analytics (map-get? growth-analytics { animal-id: animal-id })))
        
        (if (is-some analytics)
            (let
                ((data (unwrap-panic analytics))
                 (efficiency (get growth-efficiency-score data)))
                {
                    total-weight: (+ (get total-weight acc) (get current-weight data)),
                    total-gain: (+ (get total-gain acc) (get daily-gain-rate data)),
                    high-performers: (+ (get high-performers acc) (if (>= efficiency u70) u1 u0)),
                    low-performers: (+ (get low-performers acc) (if (< efficiency u40) u1 u0))
                }
            )
            acc
        )
    )
)

;; Calculate operational efficiency
(define-private (calculate-operational-efficiency (animal-count uint) 
    (metrics { total-weight: uint, total-gain: uint, high-performers: uint, low-performers: uint }))
    (if (> animal-count u0)
        (let
            ((high-performer-ratio (/ (* (get high-performers metrics) u100) animal-count))
             (growth-consistency (if (> (get total-gain metrics) u0) u50 u20)))
            (+ high-performer-ratio growth-consistency)
        )
        u0
    )
)

;; Update breed benchmarks
(define-public (update-breed-benchmark 
    (breed (string-ascii 30))
    (age-category (string-ascii 15))
    (avg-weight uint)
    (avg-daily-gain uint)
    (optimal-feed-ratio uint))
    (begin
        (map-set breed-benchmarks
            { breed: breed, age-category: age-category }
            {
                avg-weight: avg-weight,
                avg-daily-gain: avg-daily-gain,
                optimal-feed-ratio: optimal-feed-ratio,
                benchmark-health-score: u75,
                last-updated: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-performance-measurement (animal-id uint) (measurement-id uint))
    (ok (map-get? performance-measurements { animal-id: animal-id, measurement-id: measurement-id }))
)

(define-read-only (get-growth-analytics (animal-id uint))
    (ok (map-get? growth-analytics { animal-id: animal-id }))
)

(define-read-only (get-farm-metrics (farm-owner principal) (period uint))
    (ok (map-get? farm-performance-metrics { farm-owner: farm-owner, period: period }))
)

(define-read-only (get-performance-alert (animal-id uint) (alert-type (string-ascii 20)))
    (ok (map-get? performance-alerts { animal-id: animal-id, alert-type: alert-type }))
)

(define-read-only (get-breed-benchmark (breed (string-ascii 30)) (age-category (string-ascii 15)))
    (ok (map-get? breed-benchmarks { breed: breed, age-category: age-category }))
)

(define-read-only (compare-to-benchmark (animal-id uint) (breed (string-ascii 30)) (age-category (string-ascii 15)))
    (let
        ((analytics (map-get? growth-analytics { animal-id: animal-id }))
         (benchmark (map-get? breed-benchmarks { breed: breed, age-category: age-category })))
        
        (if (and (is-some analytics) (is-some benchmark))
            (let
                ((animal-data (unwrap-panic analytics))
                 (benchmark-data (unwrap-panic benchmark))
                 (weight-performance (/ (* (get current-weight animal-data) u100) (get avg-weight benchmark-data)))
                 (gain-performance (/ (* (get daily-gain-rate animal-data) u100) (get avg-daily-gain benchmark-data))))
                (ok {
                    weight-vs-benchmark: weight-performance,
                    gain-vs-benchmark: gain-performance,
                    overall-performance: (/ (+ weight-performance gain-performance) u2)
                })
            )
            err-insufficient-data
        )
    )
)
