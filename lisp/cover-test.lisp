#!/usr/bin/env slipr

;;;; Copyright (c) 2026, Peter Ohler, All rights reserved.

;;; Change directory to the location of this file.
(chdir (directory-namestring (cadr *app-args*)))

(defvar cover-suite (defsuite "cover" nil))

(defun display-help (error)
  (typecase error
    (null nil) ;; no error
    (symbol nil) ;; probably :help
    (condition (format t ">>> ~A~%~%" (slot-value error 'message)))
    (otherwise (format t ">>> ~A~%~%" error)))
  (format t "~A~%~%runs the covererage tests.~%~%usage: [<options>]~%" (cadr *app-args*))
  (format t "  -f <pattern>  filter tests~%")
  (format t "  -v            verbose test output~%")
  (format t "  -h            display help~%")
  (terpri))

(defun decolorize (str)
  (replace-all
   (replace-all
    (replace-all
     (replace-all str *ansi-reset* "R")
     *ansi-gray* "G") ;; not a function or at least no procenance
    *ansi-green* "C") ;; covered
   *ansi-red* "N")) ;; not covered

(defun run-cover-tests ()
  (recover r (display-help r)
           (let (verbose filter filter-next)
             (dolist (arg (subseq *app-args* 2))
               (cond (filter-next (setq filter arg filter-next nil))
                     ((string= arg "-v") (setq verbose t))
                     ((string= arg "-f") (setq filter-next t))
                     ((string= arg "-h") (panic :help))
                     (t (panic (format nil "~A is not a valid command line option." arg)))))
             (send cover-suite :run :verbose verbose)
             (send cover-suite :result))))

(defun cover-file (test-file cov-file)
  (send (make-command "go" "run" "../cmd/slip/main.go" "-i=false" "-c" "-" "-cover" cov-file test-file) :run))

(defun colorized-file (test-file cov-file)
  (let ((cmd (make-command "go" "run" "../cmd/slipr/main.go" "cover.lisp" cov-file test-file)))
    (with-output-to-string (s)
      (send cmd :set-stdout s)
      (send cmd :run))))

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
                        (bind colorized (colorized-file test-file cov-file))))))
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
  (deftest "function-3" suite
    (let ((line (cadddr cov-out))) ;; (... testdata/one-line.lisp" 1 14 1 20 1)
      (assert-match test-file line)
      (assert-match "1 14 1 20 1" line)))
  (deftest "colorized" suite
    (let ((lines (split (decolorize colorized) "\n")))
      (assert-match test-file (car lines))
      (assert-equal "G0001R| G;;;; some comment" (cadr lines))
      (assert-equal "G0002R| C(+ 1 (- 2 3) (- 3 2))" (caddr lines))
      (assert-equal "R------------------------------------------------------------" (cadddr lines))
      (assert-equal "Coverage: 100.0%" (nth 4 lines)))))

(let ((suite (defsuite "if" cover-suite
               :setup (lambda ()
                        (bind test-file "testdata/if-test.lisp"
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
                        (bind colorized (colorized-file test-file cov-file))))))
  `;; ;;;; a conditional test
  ;; (+ 1
  ;;    (if (< 0 1)
  ;;        (+ 2 3)
  ;;        (- 3 2)))

  (deftest "function-1" suite
    (let ((line (cadr cov-out)))
      (assert-match test-file line)
      (assert-match "1 1 1 21 1" line)))
  (deftest "function-2" suite
    (let ((line (caddr cov-out)))
      (assert-match test-file line)
      (assert-match "2 4 4 15 1" line)))
  (deftest "function-3" suite
    (let ((line (cadddr cov-out)))
      (assert-match test-file line)
      (assert-match "3 8 3 14 1" line)))
  (deftest "function-4" suite
    (let ((line (nth 4 cov-out)))
      (assert-match test-file line)
      (assert-match "4 8 4 14 0" line)))
  (deftest "colorized" suite
    (let ((lines (split (decolorize colorized) "\n")))
      (assert-match test-file (car lines))
      (assert-equal "G0001R| G;;;; a conditional test" (cadr lines))
      (assert-equal "G0002R| C(+ 1" (caddr lines))
      (assert-equal "G0003R| C   (if (< 0 1)" (nth 3 lines))
      (assert-equal "G0004R| G       C(+ 2 3)G" (nth 4 lines))
      (assert-equal "G0005R| C       N(- 3 2)C)" (nth 5 lines))
      (assert-equal "R------------------------------------------------------------" (nth 6 lines))
      (assert-equal "Coverage: 80.0%" (nth 7 lines)))))

(run-cover-tests)
