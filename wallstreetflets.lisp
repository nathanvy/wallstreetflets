(defpackage :wallstreetflets
  (:nicknames :wsf)
  (:use :cl)
  (:documentation "Wall Street FLETs: Calculate Options Greeks based on scraped data")
  (:export :compute-greeks
	   :print-greeks))

(in-package :wallstreetflets)

(defvar *six-sigma* (- 6))

(defun compute-greeks (volatility strike-price spot-price days-to-expiry &optional (put 'nil))
  "Computes the Greeks for the given Option, as specified by the arguments, and returns them as a plist.  The final argument is optional and if set to T (true) will compute a Put option, otherwise a Call is assumed.  For a function that prints the calculated Greeks to stdout see PRINT-GREEKS"
  (let* ((dte (/ days-to-expiry 365.0))
	 (r (risk-free-rate dte))
	 (d1 (d-one spot-price volatility strike-price r dte))
	 (d2 (d-two volatility dte d1)))
    (list
     'delta (delta d1 put)
     'gamma (gamma d1 volatility spot-price dte)
     'vega (vega d1 spot-price dte)
     'theta (theta d1 d2 spot-price strike-price dte volatility r put)
     'rho (rho d2 strike-price r dte put))))

(defun print-greeks (volatility strike-price spot-price days-to-expiry &optional (put 'nil))
  "Computes the Greeks for the given Option and prints them to stdout.  If you want to consume this info programmatically then you probably want the function COMPUTE-GREEKS"
  (let* ((results (compute-greeks volatility strike-price spot-price days-to-expiry put)))
    (format t "Delta: ~a~%" (getf results 'delta))
    (format t "Gamma: ~a~%" (getf results 'gamma))
    (format t "Vega: ~a~%" (getf results 'vega))
    (format t "Theta: ~a~%" (getf results 'theta))
    (format t "Rho: ~a~%" (getf results 'rho))))

(defun delta (d1 &optional (put 'nil))
  (if put
      (- (gaussian-cdf *six-sigma* d1) 1)
      (gaussian-cdf *six-sigma* d1)))

(defun gamma (d1 sigma spot tau)
  (/ (gaussian-pdf d1)
     (* spot sigma (sqrt tau))))

(defun vega (d1 spot tau)
  (* 0.01 spot (gaussian-pdf d1) (sqrt tau)))

(defun theta (d1 d2 spot strike tau sigma r &optional (put 'nil))
  (if put
      (/ (+ (/ (* (- spot) (gaussian-pdf d1) sigma)
	      (* 2 (sqrt tau)))
	   (* r
	      strike
	      (exp (- (* r tau)))
	      (gaussian-cdf *six-sigma* (- d2))))
	 365)
      (/ (- (/ (* (- spot) (gaussian-pdf d1) sigma)
		       (* 2 (sqrt tau)))
		    (* r
		       strike
		       (exp (- (* r tau)))
		       (gaussian-cdf *six-sigma* d2)))
		 365)))

(defun rho (d2 strike r tau &optional (put 'nil))
  (if put
      (* 0.01
	 (- strike)
	 tau
	 (exp (- (* r tau)))
	 (gaussian-cdf *six-sigma* (- d2)))
      (* 0.01
	 strike
	 tau
	 (exp (- (* r tau)))
	 (gaussian-cdf *six-sigma* d2))))

(defun d-one (spot sigma strike r dte)
  (/ (+ (log (/ spot strike))
	(* (+ r (/ (expt sigma 2) 2)) dte))
     (* sigma (sqrt dte))))

(defun d-two (sigma dte d1)
  (- d1 (* sigma (sqrt dte))))

;;;utility functions

(defparameter *url* "https://www.treasury.gov/resource-center/data-chart-center/interest-rates/Pages/TextView.aspx?data=yield")
(defparameter *fallback-rate* 0.001) ;0.1%

(defun fetch-yield-rates ()
  "Scrapes US Treasury Bond yields for interpolation in another function"
  (let* ((request (dex:get *url*))
	 (parsed-content (lquery:$ (lquery:initialize request)))
	 (start 118)
	 (end 130))
    (coerce (subseq (lquery:$ parsed-content ".t-chart tr td" (text)) start end) 'list)))

(defun condition-rate-list (rate-list)
  (let* ((new-list (mapcar #'(lambda (x) (/ x 100)) (mapcar #'parse-number:parse-number rate-list))))
    (push 0 new-list)))

(defun risk-free-rate (term)
  (handler-case
      (interpolate-rate term)
    (t (c)
      (values *fallback-rate* c))))

(defun interpolate-rate (term)
  "Expects a time-to-maturity/days-to-expiry value in calendar days"
  (let* ((terms '(0 0.083 0.1667 0.25 0.5 1 2 3 5 7 10 20 30))
	 (rates (condition-rate-list (fetch-yield-rates)))
	 (n (position-if #'(lambda (x) (<= (/ term 365.0) x)) terms)))
    (linear-interpolate term (elt terms n) (elt terms (+ n 1)) (elt rates n) (elt rates (+ n 1)))))

(defun linear-interpolate (x x0 x1 y0 y1)
  (/ (+ (* y0 (- x1 x)) (* y1 (- x x0)))
     (- x1 x0)))

(defun avg (a &rest rest)
  (/ (apply #'+ a rest) (+ 1 (length rest))))

(defun gaussian-pdf (x)
  "PDF of the Standard Gaussian/Normal distribution"
  (/ (exp (- (/ (expt (- x) 2) 2)))
     (sqrt (* 2 pi))))

(defun gaussian-cdf (lo hi)
  "Numeric approximation of the CDF of a gaussian/normal distribution"
  (trap-int #'(lambda (n) (gaussian-pdf n)) lo hi))

(defun trap-int (f lo hi &key (steps 2000))
  "Trapezoidal integral approximation"
  (let ((step (/ (- hi lo) steps)))
    (loop for i from lo below hi by step
	  sum
	  (* step (avg (funcall f i)
		       (funcall f (+ i step)))))))
