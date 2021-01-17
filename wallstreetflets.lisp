(defpackage :wallstreetflets
  (:nicknames :wsf)
  (:use :cl))

(in-package :wallstreetflets)

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

;;;utility functions

(defun linear-interpolate (x x0 x1 y0 y1)
  (/ (+ (* y0 (- x1 x)) (* y1 (- x x0)))
     (- x1 x0)))

(defun avg (a &rest rest)
  (/ (apply #'+ a rest) (+ 1 (length rest))))

(defun gaussian-pdf (x &key (mu 0) (sigma 1))
  "PDF of the Gaussian/Normal distribution"
  (/ (exp (- (/ (expt (- x mu) 2)
                (* 2 (expt sigma 2)))))
     (* sigma (sqrt (* 2 pi)))))

(defun gaussian-cdf (lo hi &key (mu 0) (sigma 1))
  "Numeric approximation of the CDF of a gaussian/normal distribution"
  (trap-int #'(lambda (n) (gaussian-pdf n :mu mu :sigma sigma)) lo hi))

(defun trap-int (f lo hi &key (steps 2000))
  "Trapezoidal integral approximation"
  (let ((step (/ (- hi lo) steps)))
    (loop for i from lo below hi by step
	  sum
	  (* step (avg (funcall f i)
		       (funcall f (+ i step)))))))
