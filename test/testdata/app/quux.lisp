
(defun quux ()
  (with-open-file (f "testdata/app/run-simple.out" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format f "This is the output for the AppRunSimple test.~%")))
