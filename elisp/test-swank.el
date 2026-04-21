;;; test-swank.el --- Interactive tests for SLIP's Swank server -*- lexical-binding: t -*-

;; Copyright (c) 2025, Peter Ohler, All rights reserved.

;;; Commentary:
;;
;; This file provides interactive tests for the SLIP Swank server.
;; Load this file in Emacs and use the provided functions to test
;; various Swank protocol features.
;;
;; Usage:
;;   1. Start the SLIP swank server:
;;      $ slip -e '(swank:swank-server :port 4005)'
;;
;;   2. Load this file in Emacs:
;;      M-x load-file RET path/to/test-swank.el RET
;;
;;   3. Connect to the server:
;;      M-x slip-swank-connect RET
;;
;;   4. Run tests:
;;      M-x slip-swank-test-all RET
;;
;;; Code:

(require 'slime)

(defvar slip-swank-host "127.0.0.1"
  "Host where SLIP swank server is running.")

(defvar slip-swank-port 4005
  "Port where SLIP swank server is listening.")

(defvar slip-swank-test-results nil
  "List of test results.")

;;; Connection

(defun slip-swank-connect ()
  "Connect to SLIP swank server."
  (interactive)
  (slime-connect slip-swank-host slip-swank-port))

(defun slip-swank-disconnect ()
  "Disconnect from SLIP swank server."
  (interactive)
  (slime-disconnect))

;;; Test Utilities

(defmacro slip-swank-test (name &rest body)
  "Run a test NAME with BODY, recording pass/fail."
  (declare (indent 1))
  `(condition-case err
       (progn
         (message "Running test: %s" ,name)
         ,@body
         (push (cons ,name 'pass) slip-swank-test-results)
         (message "  PASS: %s" ,name))
     (error
      (push (cons ,name (format "FAIL: %s" (error-message-string err)))
            slip-swank-test-results)
      (message "  FAIL: %s - %s" ,name (error-message-string err)))))

(defun slip-swank-test-report ()
  "Display test results."
  (interactive)
  (with-output-to-temp-buffer "*SLIP Swank Test Results*"
    (princ "SLIP Swank Server Test Results\n")
    (princ "==============================\n\n")
    (let ((passed 0) (failed 0))
      (dolist (result (reverse slip-swank-test-results))
        (if (eq (cdr result) 'pass)
            (progn
              (princ (format "PASS: %s\n" (car result)))
              (cl-incf passed))
          (princ (format "FAIL: %s\n      %s\n" (car result) (cdr result)))
          (cl-incf failed)))
      (princ (format "\nTotal: %d passed, %d failed\n" passed failed)))))

;;; Individual Tests

(defun slip-swank-test-connection-info ()
  "Test swank:connection-info."
  (interactive)
  (slip-swank-test "connection-info"
    (let ((info (slime-eval '(swank:connection-info))))
      (unless (plist-get info :pid)
        (error "Missing :pid in connection-info"))
      (unless (plist-get info :lisp-implementation)
        (error "Missing :lisp-implementation"))
      (message "    Got connection info: pid=%s, impl=%s"
               (plist-get info :pid)
               (plist-get (plist-get info :lisp-implementation) :name)))))

(defun slip-swank-test-eval-simple ()
  "Test simple evaluation."
  (interactive)
  (slip-swank-test "eval-simple"
    (let ((result (slime-eval '(swank:listener-eval "(+ 1 2)"))))
      (unless (string-match "3" (format "%s" result))
        (error "Expected 3, got %s" result))
      (message "    (+ 1 2) => %s" result))))

(defun slip-swank-test-eval-string ()
  "Test string evaluation."
  (interactive)
  (slip-swank-test "eval-string"
    (let ((result (slime-eval '(swank:listener-eval "\"hello world\""))))
      (unless (string-match "hello" (format "%s" result))
        (error "Expected hello world, got %s" result))
      (message "    \"hello world\" => %s" result))))

(defun slip-swank-test-eval-list ()
  "Test list evaluation."
  (interactive)
  (slip-swank-test "eval-list"
    (let ((result (slime-eval '(swank:listener-eval "'(1 2 3)"))))
      (message "    '(1 2 3) => %s" result))))

(defun slip-swank-test-eval-defun ()
  "Test function definition."
  (interactive)
  (slip-swank-test "eval-defun"
    (slime-eval '(swank:listener-eval "(defun test-add (a b) (+ a b))"))
    (let ((result (slime-eval '(swank:listener-eval "(test-add 10 20)"))))
      (unless (string-match "30" (format "%s" result))
        (error "Expected 30, got %s" result))
      (message "    (test-add 10 20) => %s" result))))

(defun slip-swank-test-completions ()
  "Test symbol completions."
  (interactive)
  (slip-swank-test "completions"
    (let ((result (slime-eval '(swank:completions "car" "cl-user"))))
      (unless result
        (error "No completions returned"))
      (message "    Completions for 'car': %s" (car result)))))

(defun slip-swank-test-fuzzy-completions ()
  "Test fuzzy completions."
  (interactive)
  (slip-swank-test "fuzzy-completions"
    (let ((result (slime-eval '(swank:fuzzy-completions "mlst" "cl-user"))))
      (message "    Fuzzy completions for 'mlst': %s"
               (if result (length (car result)) 0)))))

(defun slip-swank-test-apropos ()
  "Test apropos search."
  (interactive)
  (slip-swank-test "apropos"
    (let ((result (slime-eval '(swank:apropos-list-for-emacs "list"))))
      (unless result
        (error "No apropos results"))
      (message "    Apropos 'list': %d results" (length result)))))

(defun slip-swank-test-describe-symbol ()
  "Test symbol description."
  (interactive)
  (slip-swank-test "describe-symbol"
    (let ((result (slime-eval '(swank:describe-symbol "car"))))
      (unless (stringp result)
        (error "Expected string description"))
      (message "    Description of 'car': %s..."
               (substring result 0 (min 50 (length result)))))))

(defun slip-swank-test-documentation ()
  "Test documentation lookup."
  (interactive)
  (slip-swank-test "documentation"
    (let ((result (slime-eval '(swank:documentation-symbol "cons"))))
      (message "    Documentation for 'cons': %s"
               (if result
                   (substring result 0 (min 50 (length result)))
                 "(none)")))))

(defun slip-swank-test-arglist ()
  "Test argument list lookup."
  (interactive)
  (slip-swank-test "arglist"
    (let ((result (slime-eval '(swank:operator-arglist "mapcar" "cl-user"))))
      (message "    Arglist for 'mapcar': %s" result))))

(defun slip-swank-test-list-packages ()
  "Test package listing."
  (interactive)
  (slip-swank-test "list-packages"
    (let ((result (slime-eval '(swank:list-all-package-names t))))
      (unless result
        (error "No packages returned"))
      (message "    Packages: %s" (mapconcat #'identity (seq-take result 5) ", ")))))

(defun slip-swank-test-set-package ()
  "Test package switching."
  (interactive)
  (slip-swank-test "set-package"
    (let ((result (slime-eval '(swank:set-package "cl-user"))))
      (unless result
        (error "set-package returned nil"))
      (message "    Set package result: %s" result))))

(defun slip-swank-test-interactive-eval ()
  "Test interactive eval."
  (interactive)
  (slip-swank-test "interactive-eval"
    (let ((result (slime-eval '(swank:interactive-eval "(* 6 7)"))))
      (unless (string-match "42" (format "%s" result))
        (error "Expected 42, got %s" result))
      (message "    (* 6 7) => %s" result))))

;;; Run All Tests

(defun slip-swank-test-all ()
  "Run all SLIP swank tests."
  (interactive)
  (setq slip-swank-test-results nil)
  (message "Starting SLIP Swank tests...")
  (message "")

  ;; Connection tests
  (slip-swank-test-connection-info)

  ;; Evaluation tests
  (slip-swank-test-eval-simple)
  (slip-swank-test-eval-string)
  (slip-swank-test-eval-list)
  (slip-swank-test-eval-defun)
  (slip-swank-test-interactive-eval)

  ;; Completion tests
  (slip-swank-test-completions)
  (slip-swank-test-fuzzy-completions)
  (slip-swank-test-apropos)

  ;; Documentation tests
  (slip-swank-test-describe-symbol)
  (slip-swank-test-documentation)
  (slip-swank-test-arglist)

  ;; Package tests
  (slip-swank-test-list-packages)
  (slip-swank-test-set-package)

  (message "")
  (message "Tests complete!")
  (slip-swank-test-report))

;;; Interactive REPL helpers

(defun slip-swank-eval (expr)
  "Evaluate EXPR on SLIP server and display result."
  (interactive "sExpression: ")
  (let ((result (slime-eval `(swank:listener-eval ,expr))))
    (message "Result: %s" result)))

(defun slip-swank-describe (symbol)
  "Describe SYMBOL on SLIP server."
  (interactive "sSymbol: ")
  (let ((result (slime-eval `(swank:describe-symbol ,symbol))))
    (with-output-to-temp-buffer "*SLIP Describe*"
      (princ result))))

(defun slip-swank-complete (prefix)
  "Complete PREFIX on SLIP server."
  (interactive "sPrefix: ")
  (let ((result (slime-eval `(swank:completions ,prefix "cl-user"))))
    (message "Completions: %s" (car result))))

(provide 'test-swank)

;;; test-swank.el ends here
