;;; test-swank-raw.el --- Raw socket tests for SLIP's Swank server -*- lexical-binding: t -*-

;; Copyright (c) 2025, Peter Ohler, All rights reserved.

;;; Commentary:
;;
;; This file provides raw socket tests for the SLIP Swank server.
;; It doesn't require SLIME to be installed - just connects directly
;; to the server and sends/receives wire protocol messages.
;;
;; Usage:
;;   1. Start the SLIP swank server:
;;      $ slip -e '(swank:swank-server :port 4005)'
;;
;;   2. Load this file in Emacs:
;;      M-x load-file RET path/to/test-swank-raw.el RET
;;
;;   3. Run tests:
;;      M-x slip-raw-test-all RET
;;
;;; Code:

(require 'cl-lib)

(defvar slip-raw-host "127.0.0.1"
  "Host where SLIP server is running.")

(defvar slip-raw-port 4005
  "Port where SLIP server is listening.")

(defvar slip-raw-process nil
  "The network process for the connection.")

(defvar slip-raw-response nil
  "Buffer for collecting response data.")

(defvar slip-raw-continuation 1
  "Counter for RPC continuations.")

(defvar slip-raw-test-results nil
  "List of test results.")

;;; Wire Protocol

(defun slip-raw-encode-message (sexp)
  "Encode SEXP as a swank wire protocol message."
  (let* ((payload (prin1-to-string sexp))
         (length (length payload))
         (header (format "%06X" length)))
    (concat header payload)))

(defun slip-raw-decode-length (header)
  "Decode the 6-char hex HEADER to get message length."
  (string-to-number header 16))

(defun slip-raw-send (sexp)
  "Send SEXP to the server."
  (when slip-raw-process
    (process-send-string slip-raw-process (slip-raw-encode-message sexp))))

(defun slip-raw-send-rex (form &optional package)
  "Send an :emacs-rex RPC for FORM in PACKAGE."
  (let ((pkg (or package "cl-user"))
        (cont (cl-incf slip-raw-continuation)))
    (slip-raw-send `(:emacs-rex ,form ,pkg t ,cont))
    cont))

;;; Connection

(defun slip-raw-filter (proc string)
  "Process filter for incoming data from PROC with STRING."
  (setq slip-raw-response (concat slip-raw-response string)))

(defun slip-raw-sentinel (proc event)
  "Process sentinel for PROC with EVENT."
  (message "Connection event: %s" (string-trim event)))

(defun slip-raw-connect ()
  "Connect to SLIP server."
  (interactive)
  (when slip-raw-process
    (delete-process slip-raw-process))
  (setq slip-raw-response "")
  (setq slip-raw-continuation 0)
  (setq slip-raw-process
        (make-network-process
         :name "slip-raw"
         :host slip-raw-host
         :service slip-raw-port
         :filter #'slip-raw-filter
         :sentinel #'slip-raw-sentinel
         :coding 'utf-8))
  (message "Connected to %s:%d" slip-raw-host slip-raw-port))

(defun slip-raw-disconnect ()
  "Disconnect from SLIP server."
  (interactive)
  (when slip-raw-process
    (delete-process slip-raw-process)
    (setq slip-raw-process nil))
  (message "Disconnected"))

(defun slip-raw-read-response (&optional timeout)
  "Read a response from the server, waiting up to TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 5.0))))
    (while (and (< (float-time) deadline)
                (< (length slip-raw-response) 6))
      (accept-process-output slip-raw-process 0.1))
    (when (>= (length slip-raw-response) 6)
      (let* ((header (substring slip-raw-response 0 6))
             (length (slip-raw-decode-length header)))
        (while (and (< (float-time) deadline)
                    (< (length slip-raw-response) (+ 6 length)))
          (accept-process-output slip-raw-process 0.1))
        (when (>= (length slip-raw-response) (+ 6 length))
          (let* ((payload (substring slip-raw-response 6 (+ 6 length)))
                 (result (read payload)))
            (setq slip-raw-response (substring slip-raw-response (+ 6 length)))
            result))))))

(defun slip-raw-read-until-return (&optional timeout)
  "Read responses until we get a :return message."
  (let ((deadline (+ (float-time) (or timeout 5.0)))
        result)
    (while (and (< (float-time) deadline)
                (null result))
      (let ((msg (slip-raw-read-response 1.0)))
        (when msg
          (if (eq (car msg) :return)
              (setq result msg)
            (message "  Received: %S" msg)))))
    result))

;;; Test Utilities

(defmacro slip-raw-test (name &rest body)
  "Run a test NAME with BODY, recording pass/fail."
  (declare (indent 1))
  `(condition-case err
       (progn
         (message "Running test: %s" ,name)
         ,@body
         (push (cons ,name 'pass) slip-raw-test-results)
         (message "  PASS: %s" ,name))
     (error
      (push (cons ,name (format "FAIL: %s" (error-message-string err)))
            slip-raw-test-results)
      (message "  FAIL: %s - %s" ,name (error-message-string err)))))

(defun slip-raw-test-report ()
  "Display test results."
  (interactive)
  (with-output-to-temp-buffer "*SLIP Raw Test Results*"
    (princ "SLIP Raw Socket Test Results\n")
    (princ "============================\n\n")
    (let ((passed 0) (failed 0))
      (dolist (result (reverse slip-raw-test-results))
        (if (eq (cdr result) 'pass)
            (progn
              (princ (format "PASS: %s\n" (car result)))
              (cl-incf passed))
          (princ (format "FAIL: %s\n      %s\n" (car result) (cdr result)))
          (cl-incf failed)))
      (princ (format "\nTotal: %d passed, %d failed\n" passed failed)))))

;;; Individual Tests

(defun slip-raw-test-connection-info ()
  "Test connection-info RPC."
  (interactive)
  (slip-raw-test "connection-info"
    (slip-raw-send-rex '(swank:connection-info))
    (let ((response (slip-raw-read-until-return)))
      (unless response
        (error "No response received"))
      (unless (eq (car response) :return)
        (error "Expected :return, got %S" (car response)))
      (let ((result (cadr response)))
        (unless (eq (car result) :ok)
          (error "Expected :ok, got %S" result))
        (message "    Got connection info")))))

(defun slip-raw-test-eval-simple ()
  "Test simple evaluation."
  (interactive)
  (slip-raw-test "eval-simple"
    (slip-raw-send-rex '(swank:listener-eval "(+ 1 2)"))
    (let ((response (slip-raw-read-until-return)))
      (unless response
        (error "No response received"))
      (message "    Response: %S" response))))

(defun slip-raw-test-eval-string ()
  "Test string evaluation."
  (interactive)
  (slip-raw-test "eval-string"
    (slip-raw-send-rex '(swank:listener-eval "\"hello\""))
    (let ((response (slip-raw-read-until-return)))
      (unless response
        (error "No response received"))
      (message "    Response: %S" response))))

(defun slip-raw-test-completions ()
  "Test symbol completions."
  (interactive)
  (slip-raw-test "completions"
    (slip-raw-send-rex '(swank:completions "car" "cl-user"))
    (let ((response (slip-raw-read-until-return)))
      (unless response
        (error "No response received"))
      (message "    Response: %S" response))))

(defun slip-raw-test-describe ()
  "Test describe-symbol."
  (interactive)
  (slip-raw-test "describe"
    (slip-raw-send-rex '(swank:describe-symbol "car"))
    (let ((response (slip-raw-read-until-return)))
      (unless response
        (error "No response received"))
      (message "    Response: %S" (substring (format "%S" response) 0 80)))))

(defun slip-raw-test-list-packages ()
  "Test list-all-package-names."
  (interactive)
  (slip-raw-test "list-packages"
    (slip-raw-send-rex '(swank:list-all-package-names t))
    (let ((response (slip-raw-read-until-return)))
      (unless response
        (error "No response received"))
      (message "    Response: %S" response))))

(defun slip-raw-test-create-repl ()
  "Test create-repl."
  (interactive)
  (slip-raw-test "create-repl"
    (slip-raw-send-rex '(swank:create-repl nil))
    (let ((response (slip-raw-read-until-return)))
      (unless response
        (error "No response received"))
      (message "    Response: %S" response))))

;;; Run All Tests

(defun slip-raw-test-all ()
  "Run all raw socket tests."
  (interactive)
  (setq slip-raw-test-results nil)
  (message "Starting SLIP raw socket tests...")
  (message "")

  ;; Connect first
  (slip-raw-connect)
  (sleep-for 0.5)

  ;; Run tests
  (slip-raw-test-connection-info)
  (slip-raw-test-create-repl)
  (slip-raw-test-eval-simple)
  (slip-raw-test-eval-string)
  (slip-raw-test-completions)
  (slip-raw-test-describe)
  (slip-raw-test-list-packages)

  ;; Disconnect
  (slip-raw-disconnect)

  (message "")
  (message "Tests complete!")
  (slip-raw-test-report))

;;; Interactive helpers

(defun slip-raw-eval (expr)
  "Evaluate EXPR and show raw response."
  (interactive "sExpression: ")
  (unless slip-raw-process
    (slip-raw-connect)
    (sleep-for 0.5))
  (slip-raw-send-rex `(swank:listener-eval ,expr))
  (let ((response (slip-raw-read-until-return)))
    (message "Response: %S" response)))

(provide 'test-swank-raw)

;;; test-swank-raw.el ends here
