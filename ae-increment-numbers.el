; ----------------------------------------------------------------------
; This file is licensed under the GPL (v3 or later)
; Copyright 2010 Andreas Ehliar
; ----------------------------------------------------------------------


(defun string-number-first (thestring start)
  (let ((mystart (string-match "[0-9]+" (substring thestring start))))
    (if mystart (+ start mystart)
      nil)))

(defun string-number-end (thestring start)
  (string-match "[0-9]+" (substring thestring start))
  (+ start (match-end 0)))


(defun increase-all-numbers-from-previous-lines ()
  "Look at the previous two lines and insert a new line where all numbers
   that were changed in the previous line are also increased.

   Example: Given the following two lines:

   assign foo[31:24] = bar[7:0]; // Connection 0
   assign foo[23:16] = bar[15:8]; // Connection 1

   When running this function and the cursor is located after the
   final assign the following line will be inserted:

   assign foo[15:8] = bar[23:16]; // Connection 2
"
  (interactive)
  (insert
   (save-excursion
     (forward-line -1)
     (beginning-of-line)
     (let* ((difflinestart (point))
	    (difflineend (progn (end-of-line) (point)))
	    (diffline (buffer-substring-no-properties difflinestart difflineend))
	    (reflinestart (progn (forward-line -1) (beginning-of-line) (point)))
	    (reflineend (progn (end-of-line) (point)))
	    (refline (buffer-substring-no-properties reflinestart reflineend))
	    (reflen (- reflineend reflinestart))
	    (difflen (- difflineend difflinestart))
	    (i 0)
	    (diffi 0)
	    (newstring))
       
       (while (and (> reflen i) (> difflen diffi))
	 (setq numstart0 (string-number-first refline  i    ))
	 (setq numstart1 (string-number-first diffline diffi))
	 (if (and numstart0 numstart1)
	     (let* ((stringend1 (string-number-end refline  numstart0))
		    (stringend2 (string-number-end diffline numstart1))
		    (num1 (string-to-number (substring refline  numstart0 stringend1)))
		    (num2 (string-to-number (substring diffline numstart1 stringend2))))
	       (setq newstring (concat newstring (substring refline i numstart0)))
;	       (princ (format "Increasing the number to %d\n" (+ num2 (- num2 num1))))
	       (setq newstring (concat newstring (number-to-string (+ num2 (- num2 num1)))))
	       (setq i stringend1)
	       (setq diffi stringend2))
	   (progn
	     (setq newstring (concat newstring (substring refline i)))
	     (setq i reflen)
	     (setq diffi difflen))))
       (concat newstring "\n")))))



