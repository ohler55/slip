
(defun inner ()
  'inside)

(defun outer ()
  (inner))

(export 'outer)
