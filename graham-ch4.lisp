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

#2a((b nil nil) (nil nil nil))		; ILLEGAL FUNCTION CALL
