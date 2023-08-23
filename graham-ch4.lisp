;;; ANSI Common Lisp - Paul Graham - 1996
;;; 2023-08-21
;;; TQBH
;;; Chap 4 - Specialized Data Structures


;; 4.1 - Arrays
(defparameter arr (make-array '(2 3) :initial-element nil))
					; should initialize using
					; initial-element
arr

(aref arr 0 0)				; retrieve an array element
(setf (aref arr 0 0) 'b)		; replace, cannot use setq
arr

#2a((b nil nil) (nil nil nil))		; only runs in REPL, or
					; highlight and C-c C-r

(setf *print-array* nil)
arr

(setf *print-array* t)
arr

(defparameter vec (make-array 4 :initial-element nil))	; 1-D array = vector
vec

(vector "a" 'b 3)

#('a 'b 'c)				; define vector literally

(svref vec 0)
(setf (svref vec 0) 'm)
vec


;; 4.2 - Example: Binary Search
