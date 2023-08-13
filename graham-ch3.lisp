;;; ANSI Common Lisp - Paul Graham - 1996
;;; 2023-08-04
;;; TQBH
;;; Chap 3 - Lists


;; 3.1 - Conses
(defparameter x (cons 'a nil))		; one-element list

x

(car x)

(cdr x)

(defparameter z (list 'a (list 'b 'c) 'd))	; nested list, as opposed to
				        	; flat list (regular)

z

(car (cdr z))

'(a b c)
(cons 'a (cons 'b (cons 'c nil)))
(cons 'a 'b)
					
(defun our-listp (x)
  (or (null x) (consp x)))		; List is made up of conses.
					; nil is both a list and an atom
(our-listp z)

(defun our-atom (x)
  (not (consp x)))				; anything not cons is an atom

(our-atom z)


;; 3.2 - Equality
(defparameter c1 (cons 'a nil))
(defparameter c2 (cons 'a nil))
  
(eql c1 c2)				; not same object, 2 separate cons memories

(equal c1 c2)				; print same

(eql (car c1) (car c2))

(consp (car c1))

(defparameter p nil)

p

(consp p)

(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
	   (consp y)
	   (our-equal (car x) (car y))
	   (our-equal (cdr x) (cdr y)))))

(our-equal c1 c2)


;; 3.3 - Why Lisp has no pointers
(defparameter x '(a b c))

(defparameter y x)			; copies pointer to the list
					; '(a b c), not another memory
					; location for another list

(eql x y)

;; No need to create pointers in Lisp because every value is
;; conceptually a pointer to the value already, most of the time
;; (except for certain cases like an integer, stored directly as an
;; integer instead of a pointer because each option takes the same
;; amount of memory)


;; 3.4 - Building Lists
(defparameter x '(a b c))
(defparameter y (copy-list x))
y					; new bus with same passengers

(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))
(defparameter z (our-copy-list y))
z

(defparameter w (cons (car y) (cdr y)))
w

(eql (cdr y) (cdr z))
(eql (cdr y) (cdr w))

(append '(a b) '(c d) 'e)		; copies all preceding
					; arguments (except last one)
					; and append to last one


;; Meeting with Neil, 08-08-2023
(defparameter alist1 '(("foo" . "bar") ("bif" . "fred")))
(car (car alist1))

(defun alist-find (list key)
  (find-if (lambda (lst)
	     (equal key (car lst)))
	   list))

(alist-find alist1 "bif")

(find-if (lambda (lst) (equal "bif" (car lst))) alist1)
					; lst is each
					; element in sequence,
					; executed on by lambda

;; 3.5 - Example: Compression

;; Compress
(list (list 4 1))
(cons 'a 'b)
(cons '(1 2) '(3 4))

(defun compress (x)
  (if (atom x)
      x
      (compr (car x) 1 (cdr x))))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts n elt))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (1+ n) (cdr lst))
	    (cons (n-elts n elt) (compr next 1 (cdr lst)))))))

(defun n-elts (n elt)
  (if (> n 1)
      (list n elt)
      elt))

(compress '(1 1 1 0 1 0 0 0 0 1))
(compress '(1 1 1 0 1 0 0 0 0 1 1))
(compress nil)

;; Uncompress
(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
	    (rest (uncompress (cdr lst))))
	(if (consp elt)
	    (append (apply #'list-of elt) rest)
	    (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(uncompress '((3 1) 0 1 (4 0) 1))


;; 3.6 - Access
(nth 0 '(a b c))
(nthcdr 2 '(a b c d))

; without error checking
(defun our-nthcdr (n lst)
  (if (zerop n)
   lst
   (our-nthcdr (- n 1) (cdr lst))))
(our-nthcdr 2 '(a b c d))

(last '(a b c))
(nth 2 '(a b c))			; last returns a cons, while
					; nth returns an atom
(car (last '(a b c)))

(second '(a b c))
(nth 1 '(a b c))

(car (cdr '(a b c)))
(cadr '(a b c))


;; 3.7 - Mapping Functions
(mapcar #'(lambda (x) (+ x 10))
	'(1 2 3))

(mapcar #'list
	'(a b c)
	'(1 2 3))

(maplist #'(lambda (x) x)
	 '(a b c))


;; 3.8 - Trees
(copy-tree '(a (b c) d))

(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr)) (our-copy-tree (cdr tr)))))
					; slightly different from
					; copy-list

(our-copy-tree '(a (b c) d))

(substitute 'y 'x '(and (integerp x) (zerop (mod x 2)))); not work
(subst 'y 'x '(and (integerp x) (zerop (mod x 2))))	; works

(defun our-subst (new old tree)
  (if (eql tree old)
      new
      (if (atom tree)
       tree
       (cons (our-subst new old (car tree)) (our-subst new old (cdr tree))))))

(our-subst 'y 'x '(and (integerp x) (zerop (mod x 2))))


;; 3.9 - Understanding Recursion
(defun len (lst)
  (if (null lst)
      0
      (+ 1 (len (cdr lst)))))

(len nil)
(len '(a b))
(len '(a b c))
					
(defun len1 (lst acc)
  (if (null lst)
      acc
      (len1 (cdr lst) (+ 1 acc))))

(len1 nil 0)
(len1 '(a b) 0)
(len1 '(a b c) 0)


;; 3.10 - Sets
