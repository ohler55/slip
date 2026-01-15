;;; test-slynk.el --- Interactive tests for SLIP's Slynk server -*- lexical-binding: t -*-

;; Copyright (c) 2025, Peter Ohler, All rights reserved.

;;; Commentary:
;;
;; This file provides interactive tests for the SLIP Slynk server.
;; Load this file in Emacs and use the provided functions to test
;; various Slynk protocol features (compatible with Sly).
;;
;; Usage:
;;   1. Start the SLIP slynk server:
;;      $ slip -e '(slynk:slynk-server :port 4005)'
;;
;;   2. Load this file in Emacs:
;;      M-x load-file RET path/to/test-slynk.el RET
;;
;;   3. Connect to the server:
;;      M-x slip-slynk-connect RET
;;
;;   4. Run tests:
;;      M-x slip-slynk-test-all RET
;;
;;; Code:

(require 'sly)

(defvar slip-slynk-host "127.0.0.1"
  "Host where SLIP slynk server is running.")

(defvar slip-slynk-port 4005
  "Port where SLIP slynk server is listening.")

(defvar slip-slynk-test-results nil
  "List of test results.")

;;; Connection

(defun slip-slynk-connect ()
  "Connect to SLIP slynk server."
  (interactive)
  (sly-connect slip-slynk-host slip-slynk-port))

(defun slip-slynk-disconnect ()
  "Disconnect from SLIP slynk server."
  (interactive)
  (sly-disconnect))

;;; Test Utilities

(defmacro slip-slynk-test (name &rest body)
  "Run a test NAME with BODY, recording pass/fail."
  (declare (indent 1))
  `(condition-case err
       (progn
         (message "Running test: %s" ,name)
         ,@body
         (push (cons ,name 'pass) slip-slynk-test-results)
         (message "  PASS: %s" ,name))
     (error
      (push (cons ,name (format "FAIL: %s" (error-message-string err)))
            slip-slynk-test-results)
      (message "  FAIL: %s - %s" ,name (error-message-string err)))))

(defun slip-slynk-test-report ()
  "Display test results."
  (interactive)
  (with-output-to-temp-buffer "*SLIP Slynk Test Results*"
    (princ "SLIP Slynk Server Test Results\n")
    (princ "==============================\n\n")
    (let ((passed 0) (failed 0))
      (dolist (result (reverse slip-slynk-test-results))
        (if (eq (cdr result) 'pass)
            (progn
              (princ (format "PASS: %s\n" (car result)))
              (cl-incf passed))
          (princ (format "FAIL: %s\n      %s\n" (car result) (cdr result)))
          (cl-incf failed)))
      (princ (format "\nTotal: %d passed, %d failed\n" passed failed)))))

;;; Individual Tests

(defun slip-slynk-test-connection-info ()
  "Test slynk:connection-info."
  (interactive)
  (slip-slynk-test "connection-info"
    (let ((info (sly-eval '(slynk:connection-info))))
      (unless (plist-get info :pid)
        (error "Missing :pid in connection-info"))
      (unless (plist-get info :lisp-implementation)
        (error "Missing :lisp-implementation"))
      (message "    Got connection info: pid=%s, impl=%s"
               (plist-get info :pid)
               (plist-get (plist-get info :lisp-implementation) :name)))))

(defun slip-slynk-test-eval-simple ()
  "Test simple evaluation."
  (interactive)
  (slip-slynk-test "eval-simple"
    (let ((result (sly-eval '(slynk:listener-eval "(+ 1 2)"))))
      (unless (string-match "3" (format "%s" result))
        (error "Expected 3, got %s" result))
      (message "    (+ 1 2) => %s" result))))

(defun slip-slynk-test-eval-string ()
  "Test string evaluation."
  (interactive)
  (slip-slynk-test "eval-string"
    (let ((result (sly-eval '(slynk:listener-eval "\"hello world\""))))
      (unless (string-match "hello" (format "%s" result))
        (error "Expected hello world, got %s" result))
      (message "    \"hello world\" => %s" result))))

(defun slip-slynk-test-eval-list ()
  "Test list evaluation."
  (interactive)
  (slip-slynk-test "eval-list"
    (let ((result (sly-eval '(slynk:listener-eval "'(1 2 3)"))))
      (message "    '(1 2 3) => %s" result))))

(defun slip-slynk-test-eval-defun ()
  "Test function definition."
  (interactive)
  (slip-slynk-test "eval-defun"
    (sly-eval '(slynk:listener-eval "(defun test-add (a b) (+ a b))"))
    (let ((result (sly-eval '(slynk:listener-eval "(test-add 10 20)"))))
      (unless (string-match "30" (format "%s" result))
        (error "Expected 30, got %s" result))
      (message "    (test-add 10 20) => %s" result))))

(defun slip-slynk-test-completions ()
  "Test symbol completions."
  (interactive)
  (slip-slynk-test "completions"
    (let ((result (sly-eval '(slynk:completions "car" "cl-user"))))
      (unless result
        (error "No completions returned"))
      (message "    Completions for 'car': %s" (car result)))))

(defun slip-slynk-test-flex-completions ()
  "Test flex completions."
  (interactive)
  (slip-slynk-test "flex-completions"
    (let ((result (sly-eval '(slynk:flex-completions "mlst" "cl-user"))))
      (message "    Flex completions for 'mlst': %s"
               (if result (length (car result)) 0)))))

(defun slip-slynk-test-apropos ()
  "Test apropos search."
  (interactive)
  (slip-slynk-test "apropos"
    (let ((result (sly-eval '(slynk:apropos-list-for-emacs "list"))))
      (unless result
        (error "No apropos results"))
      (message "    Apropos 'list': %d results" (length result)))))

(defun slip-slynk-test-describe-symbol ()
  "Test symbol description."
  (interactive)
  (slip-slynk-test "describe-symbol"
    (let ((result (sly-eval '(slynk:describe-symbol "car"))))
      (unless (stringp result)
        (error "Expected string description"))
      (message "    Description of 'car': %s..."
               (substring result 0 (min 50 (length result)))))))

(defun slip-slynk-test-documentation ()
  "Test documentation lookup."
  (interactive)
  (slip-slynk-test "documentation"
    (let ((result (sly-eval '(slynk:documentation-symbol "cons"))))
      (message "    Documentation for 'cons': %s"
               (if result
                   (substring result 0 (min 50 (length result)))
                 "(none)")))))

(defun slip-slynk-test-arglist ()
  "Test argument list lookup."
  (interactive)
  (slip-slynk-test "arglist"
    (let ((result (sly-eval '(slynk:operator-arglist "mapcar" "cl-user"))))
      (message "    Arglist for 'mapcar': %s" result))))

(defun slip-slynk-test-list-packages ()
  "Test package listing."
  (interactive)
  (slip-slynk-test "list-packages"
    (let ((result (sly-eval '(slynk:list-all-package-names t))))
      (unless result
        (error "No packages returned"))
      (message "    Packages: %s" (mapconcat #'identity (seq-take result 5) ", ")))))

(defun slip-slynk-test-set-package ()
  "Test package switching."
  (interactive)
  (slip-slynk-test "set-package"
    (let ((result (sly-eval '(slynk:set-package "cl-user"))))
      (unless result
        (error "set-package returned nil"))
      (message "    Set package result: %s" result))))

(defun slip-slynk-test-interactive-eval ()
  "Test interactive eval."
  (interactive)
  (slip-slynk-test "interactive-eval"
    (let ((result (sly-eval '(slynk:interactive-eval "(* 6 7)"))))
      (unless (string-match "42" (format "%s" result))
        (error "Expected 42, got %s" result))
      (message "    (* 6 7) => %s" result))))

;;; MREPL (Multiple REPL) Tests

(defun slip-slynk-test-create-mrepl ()
  "Test creating an MREPL channel."
  (interactive)
  (slip-slynk-test "create-mrepl"
    (let ((result (sly-eval '(slynk:create-mrepl nil))))
      (message "    Created MREPL: %s" result))))

(defun slip-slynk-test-list-channels ()
  "Test listing channels."
  (interactive)
  (slip-slynk-test "list-channels"
    (let ((result (sly-eval '(slynk:list-channels))))
      (message "    Channels: %s" result))))

;;; Run All Tests

(defun slip-slynk-test-all ()
  "Run all SLIP slynk tests."
  (interactive)
  (setq slip-slynk-test-results nil)
  (message "Starting SLIP Slynk tests...")
  (message "")

  ;; Connection tests
  (slip-slynk-test-connection-info)

  ;; Evaluation tests
  (slip-slynk-test-eval-simple)
  (slip-slynk-test-eval-string)
  (slip-slynk-test-eval-list)
  (slip-slynk-test-eval-defun)
  (slip-slynk-test-interactive-eval)

  ;; Completion tests
  (slip-slynk-test-completions)
  (slip-slynk-test-flex-completions)
  (slip-slynk-test-apropos)

  ;; Documentation tests
  (slip-slynk-test-describe-symbol)
  (slip-slynk-test-documentation)
  (slip-slynk-test-arglist)

  ;; Package tests
  (slip-slynk-test-list-packages)
  (slip-slynk-test-set-package)

  ;; MREPL tests
  (slip-slynk-test-create-mrepl)
  (slip-slynk-test-list-channels)

  (message "")
  (message "Tests complete!")
  (slip-slynk-test-report))

;;; Interactive REPL helpers

(defun slip-slynk-eval (expr)
  "Evaluate EXPR on SLIP server and display result."
  (interactive "sExpression: ")
  (let ((result (sly-eval `(slynk:listener-eval ,expr))))
    (message "Result: %s" result)))

(defun slip-slynk-describe (symbol)
  "Describe SYMBOL on SLIP server."
  (interactive "sSymbol: ")
  (let ((result (sly-eval `(slynk:describe-symbol ,symbol))))
    (with-output-to-temp-buffer "*SLIP Describe*"
      (princ result))))

(defun slip-slynk-complete (prefix)
  "Complete PREFIX on SLIP server."
  (interactive "sPrefix: ")
  (let ((result (sly-eval `(slynk:completions ,prefix "cl-user"))))
    (message "Completions: %s" (car result))))

(provide 'test-slynk)

;;; test-slynk.el ends here
