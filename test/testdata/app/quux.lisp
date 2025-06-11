
(defun app-quux ()
  (with-open-file (f "testdata/app/run-simple.out" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format f "The variables from AppRunSimple are boo: ~A num: ~A sym: ~A str: ~A any: ~A.~%"
            boo num sym str any)))
