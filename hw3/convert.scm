;;take input in form (convert <quantity1> <unit-list2>)
;;example: (convert '(27.5 (furlong 1)(fortnight -1)) '((mi 1)(hr -1)))
;;then output is <quantity2>
;;example: (0.01023065476190476 (mi 1)(hr -1))


;;part 1: are they compatible?
         ;;example: 3 in to cm
         ;;in -> m and cm -> m so both are length and have same elem units
         ;;also ex: 2 (in 2) to (cm 1) will not work has to be the same
         ;;exponents. (m 1)(sec -2) is entirely equivalent to (sec -2)(m 1)
         ;;or even to (sec -1)(m 2)(sec -1)(m -1).
           ;;1a: use assoc on the list of elementary units to see if match
           ;;1b: add up the powers and compare if both match


;;part 2: if yes convert quantity1 to quantity2 by using the elementary units
         ;;example: 3 in to cm
         ;; 1 in = 0.0254 m
         ;; 1 cm = 0.01 m  ,so
         ;; 3 * 0.0254 in = 0.0762 m
         ;; 0.0762 m / 0.01 m = 7.62 cm


;;account for derived units

;;test cases:
;;==>(convert '(2 (in 1)) '((m 1)))
;;(0.0508 (m 1))  works

;;==> (convert '(2 (cm 2)) '((mm 2)))
;;(200 (mm 2))  works

;;==> (convert '(3 (in 1)(g 1)(in 1)) '((cm 2)(kg 1)))
;;(0.0193548 (cm 2) (kg 1))

;;==> (convert '(32999.99 (ft 1)(lbf 1)(min -1)) '((hp 1)))
;;(0.9999996974749941 (hp 1))

;;==> (convert '(5 (joule 2)(N -3)(hr 1)) '((g -1)(m 1)(min 3)))
;;(8.333333333333334e-05 (g -1) (m 1) (min 3))

;;==> (convert '(23 (g -1)(m 1)(min 3)) '((joule 2)(N -3)(hr 1)))
;;(1380000.0 (joule 2) (n -3) (hr 1))

;;-------------------------------------------------------

(define (convert quantity1 unit-list2)
  (let ((unit-list1 (cdr quantity1)) ;;unit list from quantity1
	(val1 (car quantity1))) ;;value from quantity1
    (let ((elem1 (get-elem unit-list1)) ;;list of elemental units
	  (elem2 (get-elem unit-list2)) ;;list of elemental units
	  (convert1 (get-conv-list unit-list1));;helper list of what
	  ;;their conversion values are and their exponent to be used
	  ;;in the calculation procedure
	  (convert2 (get-conv-list unit-list2)))
      (if (and (units-match? elem1 elem2) (= (counter elem1) (counter elem2)))
	  (cons (* val1 (calc convert1 convert2)) unit-list2)
	  (error "Cannot convert these units")))))
	  



;;-------------------------------------------------------

;;helper function to get the whole list of elementary units

(define (get-elem unit-list)
 
  (if (null? unit-list)
      '()
      (if (assoc (caar unit-list) source)  ;;check if not already elemental
	  (let ((a (assoc (caar unit-list) source)))  ;;search through source
	    (let ((b (cadr a)))
	      (let ((c (cdr b)))
		(if (= (cadar unit-list) 1) ;;if the unit we are getting elem
		  ;;units for is a power of one leave it, otherwise we need to
		  ;;make the elem units * the power of this unit
		  ;;ex: (cm 2) needs to be (m 2)
		    (append c (get-elem (cdr unit-list)))
	            (append (elem-power c (cadar unit-list))
			    (get-elem (cdr unit-list)))))))
	  ;;if false/cant find in list then return the value itself
	  (cons (car unit-list) (get-elem (cdr unit-list))))))


;;tested: works
;;==> (get-elem '((N 1) (m 1)))
;;((kg 1) (m 1) (sec -2) (m 1))

;;==> (get-elem '((in 2)))
;;((m  2))

;;==> (get-elem '((in 1)(g 1)))
;;((m 1) (kg 1))

;;==> (get-elem '((N 2)))
;;((kg 2) (m 2) (sec -4))

;;==> (get-elem '((hp -2) (sec -2)))
;;((kg -2) (m -4) (sec 6) (sec -2))

;;helper function to get-elem for changing the exponents to match unit
	   
(define (elem-power elem-list unit-power)
  (if (null? elem-list)
      '()
      (cons
       (list (caar elem-list)
	     (* (cadar elem-list) unit-power))
       (elem-power (cdr elem-list) unit-power))))

	    
;;--------------------------------------------------------


;;helper function to see if elem units match (U-normalized = V-normalized)
;;and if they have the same exponents
(define (units-match? elem1 elem2)
  (if (null? elem1)
      #t
      (if (assoc (caar elem1) elem2);;is unit in 2nd list
	  (units-match? (cdr elem1) elem2) ;;loop
	  #f)))


;;helper for units-match?
;;example: '((m 1) (sec -1) (m 1)) = '((m 2) (sec -1))
(define (counter elem-list)
  (if (null? elem-list)
      0
      (+ (cadar elem-list) (counter (cdr elem-list)))))


;;--------------------------------------------------------
;;helper function to get elemental value thats equal to one base unit
(define (get-conv-val base-unit)
  ;;data file entry is in form (in (0.0254 (m 1)))

  (if (assoc (car base-unit) source) ;;if not elem base already
      (let ((a (assoc (car base-unit) source))) 
	(let((b (cadr a)))
	  (car b)))
      1)) ;;if it is elemental base already just use 1 as value
      

;;tested: works
;;
;;==> (get-conv-val '(in))
;;0.0254
;;==> (get-conv-val '(m))
;;1

;;--------------------------------------------------------

;;helper function to get the whole list of the elementary conversion values
;; it includes the exponent for use in calculations
(define (get-conv-list unit-list)
  (if (null? unit-list)
      '()
      (cons (cons (get-conv-val (car unit-list)) (cdr(car unit-list)))
	    (get-conv-list(cdr unit-list)))))


;;tested: works
;;==> (get-conv-list '((in 1)(g 1)))
;;((0.0254 1) (0.001 1))     

;;--------------------------------------------------------

;;helper functions for the calculations

;;helper to get a number ^ power
(define (power number pow)
  (if (positive? pow)  
      (if (= pow 0)
	  1
	  (* (car number) (power number (- pow 1))))
      (if (= pow 0) ;;else if negative reverse counter
	  1
	  (* (car number) (power number (+ pow 1))))))
	  

;;helper to make the conv-list account for powers
(define (power-list conv-list)
  (if (null? conv-list)
      '()
      (cons
       (list
	(power (car conv-list) (cadar conv-list))
	(cadar conv-list))
       (power-list (cdr conv-list)))))
  

;;gets the numerator by recursivley multiplying the positive exponents
(define (numerator conv-list)
  (if (null? conv-list)
	1
	(if (positive? (cadar conv-list)) ;;account for powers
	    (* (caar conv-list) (numerator (cdr conv-list)))
	    (numerator (cdr conv-list)))))


;;gets the denominator by recursivley multiplying the negative exponents
(define (denom conv-list)
    (if (null? conv-list)
	1
	(if (negative? (cadar conv-list))
	    (* (caar conv-list) (denom (cdr conv-list)))
	    (denom (cdr conv-list)))))

;;calculation
(define (calc conv-list1 conv-list2)
  (let ((pow1 (power-list conv-list1))
	(pow2 (power-list conv-list2)))
    (let ((num1 (numerator pow1))
	  (den1 (denom pow1))
	  (num2 (numerator pow2))
	  (den2 (denom pow2)))
      (/(/ num1 den1) (/ num2 den2)))))
      


;;--------------------------------------------------------

;; read-file produces a list whose elements are the expressions in the file.

(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

;; Here we go:  read in the database.

(define source (with-input-from-file "units.dat" read-file))
