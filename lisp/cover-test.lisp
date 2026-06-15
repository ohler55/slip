#!/usr/bin/env slipr

;;;; Copyright (c) 2026, Peter Ohler, All rights reserved.

;;; Change directory to the location of this file.
(chdir (directory-namestring (cadr *app-args*)))

(defvar cover-suite (defsuite "cover" nil))

(defun run-cover-tests ()
  (let ((verbose (member "-v" *app-args*)))
    (send cover-suite :run :verbose verbose)
    (send cover-suite :result)))

(defun cover-file (test-file cov-file)
  (send (make-command "go" "run" "../cmd/slip/main.go" "-i=false" "-c" "-" "-cover" cov-file test-file) :run))

(let ((suite (defsuite "one-line" cover-suite
               :setup (lambda ()
                        (bind test-file "testdata/one-line.lisp"
                              cov-file "testdata/cov.lisp")
                        (cover-file test-file cov-file)
                        (bind cov-out
                              (let (out)
                                (with-open-file (f cov-file :direction :input)
                                  (loop
                                   (let ((line (nth-value 0 (read-line f nil nil))))
                                     (unless line (return 'eof))
                                     (addf out line))))
                                out))
                        ;; TBD colorized
                        ))))
  ;;  ;;;; some comment
  ;;  (+ 1 (- 2 3) (- 3 2))

  (deftest "file-list" suite
    (assert-match test-file (car cov-out))) ;; should list the test file.

  (deftest "function-1" suite
    (let ((line (cadr cov-out))) ;; (... testdata/one-line.lisp" 1 1 1 21 1)
      (assert-match test-file line)
      (assert-match "1 1 1 21 1" line)))

  (deftest "function-2" suite
    (let ((line (caddr cov-out))) ;; (... testdata/one-line.lisp" 1 6 1 12 1)
      (assert-match test-file line)
      (assert-match "1 6 1 12 1" line)))
  (deftest "function-2" suite
    (let ((line (cadddr cov-out))) ;; (... testdata/one-line.lisp" 1 14 1 20 1)
      (assert-match test-file line)
      (assert-match "1 14 1 20 1" line))))


(run-cover-tests)
