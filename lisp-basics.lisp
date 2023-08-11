;;; ANSI Common Lisp - Paul Graham - 1996
;;; 2023-07-30,31
;;; TQBH
;;; Chap 2 - Welcome to Lisp


;;; 2.2- Evaluations
(+ 4 3 1)
8

(/ (+ 5 3) (- 6 4))
4

'(+ 8 1)				; quote to protect an expression
(+ 8 1)


;;; 2.3 - Data
3
3					; integer

"this is a string"
"this is a string"			; string

'Artichoke
Artichoke				; symbol (word)

'(my 3 "sons")
(my 3 "sons")				; list

'(the list (a b c) has 3 elements)
(the list (a b c) has 3 elements)	; list

(list 'my (+ 2 1) "sons")
(my 3 "sons")				; list

(list '(+ 2 1) (+ 2 1))			; list
((+ 2 1) 3)

()					; empty list
nil

nil					; empty list (evaluates to itself)
nil


;;; 2.4 - List Operations
(cons 'a '(b c d))			; build a list
(a b c d)

(cons 'a (cons 'b nil))
(a b)

(list 'a 'b)
(a b)

(car '(a b c))				; contents of address of register
a

(cdr '(a b c))				; contents of decrement of register
(b c)

(car (cdr (cdr '(a b c d))))
c

(nth 0 '(a b c d))
a

(nth 2 '(a b c d))
c


;;; 2.5 - Truth
(listp '(a b c))			; listp checks if a list. t means true
t					; p = predicate

(listp 27)				; predicate, false
nil

(null nil)				; true if an empty list
t

(not nil)				; negation of argument
t

(if (listp '(a b c))
    (+ 1 2)
  (+ 5 6))
3

(if (listp 27)
    (+ 1 2)
  (+ 5 6))
11

(if (listp 27)
    (+ 1 2))
nil

(if 27 1 2)				; everything except nil counts as true
1

(and 1 3 5 t 7)
7
					; if all arguments true, (and)
					; returns last condition


;;; 2.6 - Functions
(defun our-third (x)			; define a function
  (car (cdr (cdr x))))
our-third

(our-third '(a b c d))			; call the defined function
c

(defun sum-greater (x y z)
  (> (+ x y) z))
sum-greater

(sum-greater 4 1 2)
t


;;; 2.7 - Recursion
(defun our-member (obj lst)
  (if (null lst)
      nil
    (if (eql obj (car lst))
	lst
      (our-member obj (cdr lst)))))
our-member

(our-member 'b '(a b c))
(b c)

(our-member 'z '(a b c))
nil


;;; 2.8 - Reading Lisp
;; just about reading Lisp code


;;; 2.9 - Input and Output
(format "%d plus %d equals %d." 2 3 (+ 2 3))	; output
"2 plus 3 equals 5."

(format "%d 
plus %d 
equals %d." 2 3 (+ 2 3))
"2 
plus 3 
equals 5."

(defun askem (string)
  (format "%s" string))
askem

(askem "How old are you? ")
"How old are you? "

(defun askem1 (string)
  (print (format "%s" string))
  (read))
askem1

(askem1 "How old are you? ")		
"How old are you? "

99


;; 2.10 - Variables
(let ((x 1) (y 2))
  (+ x y))
3

(defun ask-number ()
  (print (format "Please enter a number. "))
  (let ((val (read)))
    (if (numberp val)
	val
      (ask-number))))
ask-number

(ask-number)

"Please enter a number. "

"Please enter a number. "

"Please enter a number. "

"Please enter a number. "
22

(defparameter *glob* 999)		; defparameter does not work

(defvar *glob* 999)
*glob*					; global variable

*glob*
999

(defconst limit (+ *glob* 1))
limit					; global constant

limit
1000

(boundp '*glob*)
t					; check if global var/const

(boundp 'limit)
t


;;; 2.11 - Assignment
(setf *glob* 98)
98

(let ((n 10))
  (setf n 2)
  n)
2


(setf xg (list 'a 'b 'c))
(a b c)					; xg is not a local variable,
					; so implicitly assigned as a
					; new global variable

(setf (car xg) 'n)
n

xg
(n b c)

(setf a 1
      b 2
      c 3)
3

(format "%d %d %d" a b c)
"1 2 3"


;;; 2.12 - Functional Programming (means to avoid using setf and the like)
(setf lst '(c a r a t))
(c a r a t)

(remove 'a lst) 
(c r t)					; returns a list without a but
					; did not alter lst

lst
(c a r a t)				; not altered after remove

(setf lst (remove 'a lst))		; re-assign to actually alter
(c r t)

lst
(c r t)


;;; 2.13 - Iteration
(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
      (format t "~A ~A~%" i (* i i))))

(show-squares 2 5)

(defun show-squares1 (start end)
  (while (<= start end)
    (princ (format "%d %d" start (* start start)))
    (terpri)				; go to new line
    (setq start (+ start 1))))
show-squares1

(show-squares1 2 5)
2 4
3 9
4 16
5 25
nil

(defun show-squares-recur (s e)	     	; s = start, e = end
  (if (> s e)
      nil
    (progn
      (princ (format "%d %d" s (* s s)))
      (terpri)
      (show-squares-recur (+ s 1) e))))
show-squares-recur

(show-squares-recur 2 5)
2 4
3 9
4 16
5 25
nil

;; dolist
(setq lst '(a b c d e))
(a b c d e)

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setq len (+ len 1)))
    len))
our-length

(our-length lst)
5

;; recursive version of our-length
(defun our-length-recur (lst)
  (if (null lst)
      0
    (+ 1 (our-length-recur (cdr lst)))))
our-length-recur

(our-length-recur lst)
5

;; tail-recursive version of our-length
(defun our-length-tlrec (lst)
  (cl-labels ((len (lst acc)		; cl-labels allows recursive
		(if (null lst)
		    acc
		  (len (cdr lst) (1+ acc)))))
	  (len lst 0)))
our-length-tlrec

(our-length-tlrec lst)
5


;;; 2.14 - Functions as Objects
(function +)
+

#'+
+
					; #' is same as (function)

(apply #'+ '(1 2 3))
6

(+ 1 2 3)
6

(apply #'+ 1 2 '(3 4 5))
15					; any number of args, as long
					; as the last is a list

(funcall #'+ 1 2 3 4 5)
15
					; funcall same as apply but no
					; need for list args

(lambda (x y)
  (+ x y))
(closure (*glob* limit t) (x y) (+ x y)); lambda to specify function
					; , no name necessary

(lambda (x) (+ x 100))
(closure (*glob* limit t) (x) (+ x 100))

((lambda (x) (+ x 100)) 1)
101

(funcall #'(lambda (x) (+ x 100))
	 1)
101


;; 2.15 - Types
;; In Lisp, variables have no specified type. Variables can take
;; different types of values/objects. Type declaration is only
;; necessary for reasons of efficiency.
(integerp 27)
t














