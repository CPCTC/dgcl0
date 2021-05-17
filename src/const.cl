(provide 'const)

(defun move-dir (coords dir)
  (let ((add (elt '((0 1) (-1 0) (0 -1) (1 0)) dir)))
    (mapcar #'+ coords add)))

(defconstant drcall->char
  ((lambda ()
    (let ((hash (make-hash-table)))
      (setf (gethash nil hash) #\.)
      hash)))
  "Constant hash table for converting drcalls to characters.
   Can be treated as a injective function.
   Input: A drcall function, or nil for 'unknown'.
   Output: The drcall's coresponding character.")

(defconstant bullet-char #\*)
