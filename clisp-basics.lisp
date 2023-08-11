;;; ANSI Common Lisp - Paul Graham - 1996
;;; 2023-08-01,02
;;; TQBH
;;; Chap 2 - Welcome to Lisp


;;; 2.2- Evaluations
(+ 4 3 1)

(/ (+ 5 3) (- 6 4))

'(+ 8 1)				; quote to protect an expression

;;; 2.3 - Data
3					; integer

"this is a string"			; string

'Artichoke				; symbol (word)

'(my 3 "sons")				; list

'(the list (a b c) has 3 elements)	; list

(list 'my (+ 2 1) "sons")		; list

(list '(+ 2 1) (+ 2 1))			; list

()					; empty list

nil					; empty list (evaluates to itself)


;;; 2.4 - List Operations
(cons 'a '(b c d))			; build a list

(cons 'a (cons 'b nil))

(list 'a 'b)

(car '(a b c))				; contents of address of register

(cdr '(a b c))				; contents of decrement of register

(car (cdr (cdr '(a b c d))))

(nth 0 '(a b c d))

(nth 2 '(a b c d))

(third '(a b c d))

(second '(a b c d))


;;; 2.5 - Truth
(listp '(a b c))			; listp checks if a list. t means true
					; p = predicate

(listp 27)				; predicate, false (NIL)

(null nil)				; true if an empty list

(not nil)				; negation of argument

(if (listp '(a b c))
    (+ 1 2)
    (+ 5 6))

(if (listp 27)
    (+ 1 2)
    (+ 5 6))

(if (listp 27)
    (+ 1 2))

(if 27 1 2)				; everything except nil counts as true

(and 1 3 5 t 7)				; if all arguments true, (and)
					; returns last condition


;;; 2.6 - Functions
(defun our-third (x)			; define a function
  (car (cdr (cdr x))))

(our-third '(a b c d))			; call the defined function

(defun sum-greater (x y z)
  (> (+ x y) z))

(sum-greater 4 1 2)


;;; 2.7 - Recursion
(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql obj (car lst))
	  lst
	  (our-member obj (cdr lst)))))

(our-member 'b '(a b c))

(our-member 'z '(a b c))


;;; 2.8 - Reading Lisp
;; just about reading Lisp code


;;; 2.9 - Input and Output
(format t "~D plus ~D equals ~D.~%" 2 3 (+ 2 3))	; output

(format t "~D plus ~%~D ~%equals ~D.~%" 2 3 (+ 2 3))

(defun askem (string)
  (format t "~A" string)
  (finish-output)
  (read))

(askem "How old are you? ")


;; 2.10 - Variables
(let ((x 1) (y 2))
  (+ x y))

(defun ask-number ()
  (format t "Please enter a number. ")
  (finish-output)
  (let ((val (read)))
    (if (numberp val)
	val
      (ask-number))))

(ask-number)

(defparameter *glob* 999)		; defparameter does not work

(defvar *glob* 999)			; global variable

*glob*

(defconstant limit (+ *glob* 1))		; global constant

limit

(boundp '*glob*)				; check if global var/const

(boundp 'limit)


;;; 2.11 - Assignment
(setf *glob* 98)

(let ((n 10))
  (setf n 2)
  n)

(setf xg (list 'a 'b 'c))		; xg is not a local variable,
					; so implicitly assigned as a
					; new global variable

(setf (car xg) 'n)

xg

(setf a 1
      b 2
      c 3)

(format t "~%~A ~A ~A~%" a b c)


;;; 2.12 - Functional Programming (means to avoid using setf and the like)
(setf lst '(c a r a t))

(remove 'a lst)				; returns a list without a but
					; did not alter lst

lst					; not altered after remove

(setf lst (remove 'a lst))		; re-assign to actually alter

lst


;;; 2.13 - Iteration
(defun show-squares (start end)
  (do ((i start (1+ i)))
      ((> i end) (format t "DONE~%"))
    (format t "~A ~A~%" i (* i i))))

(show-squares 2 5)

(defun show-squares-recur (s e)	     	; s = start, e = end
  (if (> s e)
      (format t "DONE~%")
      (progn
	(format t "~A ~A~%" s (* s s))
	(show-squares-recur (+ s 1) e))))

(show-squares-recur 2 5)

;; dolist
(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setq len (+ len 1)))
    len))

(our-length lst)

;; recursive version of our-length
(defun our-length-recur (lst)
  (if (null lst)
      0
      (+ 1 (our-length-recur (cdr lst)))))

(our-length-recur lst)

;; tail-recursive version of our-length
(defun our-length-tlrec (lst)
  (labels ((len (lst acc)		; labels allows recursive
	     (if (null lst)
		 acc
		 (len (cdr lst) (1+ acc)))))
    (len lst 0)))

(our-length-tlrec lst)

(defun our-length-tlrec1 (lst acc)
  (if (null lst)
      acc
      (our-length-tlrec1 (cdr lst) (1+ acc))))

(our-length-tlrec1 lst 0)

;;; 2.14 - Functions as Objects
(function +)

#'+					; #' is same as (function)

(apply #'+ '(1 2 3))

(+ 1 2 3)

(apply #'+ 1 2 '(3 4 5))		; any number of args, as long
					; as the last is a list

(funcall #'+ 1 2 3 4 5)			; funcall same as apply but no
					; need for list args

(lambda (x y)
  (+ x y))				; lambda to specify function
					; , no name necessary

((lambda (x) (+ x 100)) 1)

(funcall #'(lambda (x) (+ x 100))
	 1)

(defun f1 (x)
  (* x x))				; define in function namespace

(defparameter f1 (lambda (x) (+ x x)))	; define in variable namespace

(f1 3)					; call in function namespace

(funcall #'f1 3)			; call in function namespace

(funcall f1 3)				; call in variable namespace

;; 2.15 - Types
;; In Lisp, variables have no specified type. Variables can take
;; different types of values/objects. Type declaration is only
;; necessary for reasons of efficiency.
(integerp 27)














