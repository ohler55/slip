
(defun quux ()
  (with-open-file (f "testdata/make-app.out" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format f "make-app ran with num argument of ~D~%" num)))
