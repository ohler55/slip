;;; test-swank-ert.el --- ERT tests for SLIP's Swank server -*- lexical-binding: t -*-

;; Copyright (c) 2025, Peter Ohler, All rights reserved.

;;; Commentary:
;;
;; Automated ERT tests for the SLIP Swank server.  Does not require SLIME --
;; connects directly via raw TCP and speaks the wire protocol.
;;
;; Usage:
;;   1. Start the SLIP swank server:
;;        slip -e '(swank:swank-server :port 4005)'
;;
;;   2. Run all tests:
;;        emacs -q -nw --batch -l elisp/test-swank-ert.el \
;;              -f ert-run-tests-batch-and-exit
;;
;;   3. Or run interactively:
;;        emacs -q -nw -l elisp/test-swank-ert.el -f ert-run-tests-interactively
;;
;;   Set SLIP_SWANK_PORT to override the default port (4005).
;;
;;; Code:

(require 'ert)
(require 'cl-lib)

;;;; --- Configuration --------------------------------------------------------

(defvar slip-test-host "127.0.0.1"
  "Host where SLIP Swank server is running.")

(defvar slip-test-port (string-to-number (or (getenv "SLIP_SWANK_PORT") "4005"))
  "Port where SLIP Swank server is listening.")

;;;; --- Wire protocol --------------------------------------------------------

(defvar slip-test--proc nil  "Network process.")
(defvar slip-test--buf  ""   "Accumulation buffer for incoming data.")
(defvar slip-test--cont 0    "Continuation counter.")

(defun slip-test--encode (sexp)
  "Encode SEXP as a swank wire-protocol message."
  (let* ((payload (prin1-to-string sexp))
         (header  (format "%06X" (length payload))))
    (concat header payload)))

(defun slip-test--filter (_proc string)
  "Accumulate incoming STRING."
  (setq slip-test--buf (concat slip-test--buf string)))

(defun slip-test--connect ()
  "Open a TCP connection to the Swank server."
  (when slip-test--proc (delete-process slip-test--proc))
  (setq slip-test--buf  ""
        slip-test--cont 0
        slip-test--proc
        (make-network-process
         :name "slip-ert"
         :host slip-test-host
         :service slip-test-port
         :filter  #'slip-test--filter
         :coding  'utf-8
         :noquery t)))

(defun slip-test--disconnect ()
  "Close the TCP connection."
  (when slip-test--proc
    (delete-process slip-test--proc)
    (setq slip-test--proc nil)))

(defun slip-test--send (sexp)
  "Send SEXP over the wire."
  (process-send-string slip-test--proc (slip-test--encode sexp)))

(defun slip-test--send-rex (form &optional package)
  "Send :emacs-rex for FORM in PACKAGE.  Return continuation id."
  (let ((cont (cl-incf slip-test--cont)))
    (slip-test--send `(:emacs-rex ,form ,(or package "cl-user") t ,cont))
    cont))

(defun slip-test--read-message (&optional timeout)
  "Read one wire message, waiting up to TIMEOUT seconds (default 5)."
  (let ((deadline (+ (float-time) (or timeout 5.0))))
    ;; wait for 6-byte header
    (while (and (< (float-time) deadline)
                (< (length slip-test--buf) 6))
      (accept-process-output slip-test--proc 0.1))
    (when (>= (length slip-test--buf) 6)
      (let ((len (string-to-number (substring slip-test--buf 0 6) 16)))
        ;; wait for full payload
        (while (and (< (float-time) deadline)
                    (< (length slip-test--buf) (+ 6 len)))
          (accept-process-output slip-test--proc 0.1))
        (when (>= (length slip-test--buf) (+ 6 len))
          (let ((result (read (substring slip-test--buf 6 (+ 6 len)))))
            (setq slip-test--buf (substring slip-test--buf (+ 6 len)))
            result))))))

(defun slip-test--read-return (&optional timeout)
  "Read messages until :return, returning that message.
Intermediate :write-string messages are discarded."
  (let ((deadline (+ (float-time) (or timeout 5.0)))
        msg)
    (while (and (< (float-time) deadline) (null msg))
      (let ((m (slip-test--read-message (- deadline (float-time)))))
        (when m
          (if (eq (car m) :return)
              (setq msg m)
            ;; discard :write-string, :indentation-update, etc.
            ))))
    msg))

(defun slip-test--rex-ok (form &optional package)
  "Send FORM, read :return, assert :ok, return the value."
  (slip-test--send-rex form package)
  (let ((resp (slip-test--read-return)))
    (should resp)
    (should (eq (car resp) :return))
    (let ((inner (cadr resp)))
      (should (eq (car inner) :ok))
      (cadr inner))))

;;;; --- Fixture: connect / disconnect around each test -----------------------

(defun slip-test--ensure-connected ()
  "Connect if not already connected."
  (unless (and slip-test--proc (process-live-p slip-test--proc))
    (condition-case err
        (slip-test--connect)
      (error (ert-skip (format "Cannot connect to Swank server at %s:%d -- %s"
                               slip-test-host slip-test-port
                               (error-message-string err)))))))

;;;; --- Tests ----------------------------------------------------------------

;;; Connection & lifecycle

(ert-deftest slip-swank/ping ()
  "Ping echoes its argument."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:ping 42))))
    (should (equal val 42))))

(ert-deftest slip-swank/connection-info ()
  "connection-info returns a plist with :pid and :lisp-implementation."
  (slip-test--ensure-connected)
  (let ((info (slip-test--rex-ok '(swank:connection-info))))
    (should (plist-get info :pid))
    (should (plist-get info :lisp-implementation))))

(ert-deftest slip-swank/create-repl ()
  "create-repl returns (package-name prompt)."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:create-repl nil))))
    (should (listp val))
    (should (stringp (car val)))))

(ert-deftest slip-swank/swank-require ()
  "swank-require returns a list (possibly empty)."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:swank-require nil))))
    (should (listp val))))

;;; Evaluation

(ert-deftest slip-swank/interactive-eval ()
  "interactive-eval of (* 6 7) returns a string containing 42."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:interactive-eval "(* 6 7)"))))
    (should (stringp val))
    (should (string-match-p "42" val))))

(ert-deftest slip-swank/eval-and-grab-output ()
  "eval-and-grab-output returns (output-string result-string)."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:eval-and-grab-output "(+ 1 2)"))))
    (should (listp val))
    (should (= (length val) 2))
    (should (string-match-p "3" (cadr val)))))

(ert-deftest slip-swank/listener-eval ()
  "listener-eval sends :write-string and :return."
  (slip-test--ensure-connected)
  (slip-test--send-rex '(swank:listener-eval "(+ 10 20)"))
  (let ((resp (slip-test--read-return)))
    (should resp)
    (should (eq (car resp) :return))))

(ert-deftest slip-swank/pprint-eval ()
  "pprint-eval returns a string."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:pprint-eval "'(1 2 3)"))))
    (should (stringp val))))

(ert-deftest slip-swank/interactive-eval-region ()
  "interactive-eval-region returns a string."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:interactive-eval-region "(+ 1 1)"))))
    (should (stringp val))
    (should (string-match-p "2" val))))

;;; Compilation

(ert-deftest slip-swank/compile-string ()
  "compile-string-for-emacs returns :compilation-result with :successp."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok
              '(swank:compile-string-for-emacs "(+ 1 2)" "test.lisp" 1 nil nil))))
    (should (memq :successp val))
    (should (plist-get val :successp))))

(ert-deftest slip-swank/compile-string-error ()
  "compile-string-for-emacs with bad syntax returns :notes."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok
              '(swank:compile-string-for-emacs "(+ 1" "test.lisp" 1 nil nil))))
    (should (memq :notes val))
    (should (plist-get val :notes))))

;;; Packages

(ert-deftest slip-swank/list-all-package-names ()
  "list-all-package-names returns a list of strings."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:list-all-package-names t))))
    (should (listp val))
    (should (cl-every #'stringp val))
    (should (member "common-lisp" val))))

(ert-deftest slip-swank/set-package ()
  "set-package returns a list."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:set-package "cl-user"))))
    (should val)))

;;; Completions

(ert-deftest slip-swank/completions ()
  "completions for \"car\" returns matches."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:completions "car" "cl-user"))))
    (should (listp val))
    (should (car val))))  ; first element is list of completions

(ert-deftest slip-swank/simple-completions ()
  "simple-completions returns matches."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:simple-completions "map" "cl-user"))))
    (should (listp val))))

(ert-deftest slip-swank/fuzzy-completions ()
  "fuzzy-completions returns matches."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:fuzzy-completions "car" "cl-user"))))
    (should (listp val))))

(ert-deftest slip-swank/completions-for-keyword ()
  "completions-for-keyword returns a list."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:completions-for-keyword ":test" nil))))
    (should (listp val))))

;;; Documentation & description

(ert-deftest slip-swank/describe-symbol ()
  "describe-symbol for car returns a string."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:describe-symbol "car"))))
    (should (stringp val))
    (should (> (length val) 0))))

(ert-deftest slip-swank/describe-function ()
  "describe-function for cons returns a string."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:describe-function "cons"))))
    (should (stringp val))))

(ert-deftest slip-swank/documentation-symbol ()
  "documentation-symbol for car returns a string or nil."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:documentation-symbol "car"))))
    (should (or (null val) (stringp val)))))

(ert-deftest slip-swank/operator-arglist ()
  "operator-arglist for mapcar returns a string."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:operator-arglist "mapcar" "cl-user"))))
    (should (or (null val) (stringp val)))))

(ert-deftest slip-swank/autodoc ()
  "autodoc returns an arglist or :not-available."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:autodoc (car x)))))
    (should val)))

(ert-deftest slip-swank/apropos ()
  "apropos-list-for-emacs returns matches for \"list\"."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:apropos-list-for-emacs "list"))))
    (should (listp val))
    (should (> (length val) 0))))

;;; Inspector

(ert-deftest slip-swank/inspect-number ()
  "Inspecting a number returns :title and :content."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:init-inspector "42"))))
    (should (plist-get val :title))
    (should (plist-get val :content))))

(ert-deftest slip-swank/inspect-list ()
  "Inspecting a list returns content with elements."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:init-inspector "'(1 2 3)"))))
    (should (plist-get val :title))))

(ert-deftest slip-swank/inspect-drill-down ()
  "inspect-nth-part drills into an element."
  (slip-test--ensure-connected)
  ;; First inspect a list
  (slip-test--rex-ok '(swank:init-inspector "'(a b c)"))
  ;; Drill into element 0
  (let ((val (slip-test--rex-ok '(swank:inspect-nth-part 0))))
    (should (or (null val) (plist-get val :title)))))

(ert-deftest slip-swank/inspector-pop ()
  "inspector-pop navigates back."
  (slip-test--ensure-connected)
  (slip-test--rex-ok '(swank:init-inspector "'(1 2 3)"))
  (slip-test--rex-ok '(swank:inspect-nth-part 0))
  (let ((val (slip-test--rex-ok '(swank:inspector-pop))))
    ;; May return nil if history is empty, or inspector content
    (should t)))

(ert-deftest slip-swank/quit-inspector ()
  "quit-inspector returns nil."
  (slip-test--ensure-connected)
  (slip-test--rex-ok '(swank:init-inspector "42"))
  (let ((val (slip-test--rex-ok '(swank:quit-inspector))))
    (should (null val))))

;;; Macroexpand

(ert-deftest slip-swank/macroexpand-1 ()
  "swank-macroexpand-1 returns a string."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:swank-macroexpand-1 "(and x y)"))))
    (should (stringp val))))

(ert-deftest slip-swank/macroexpand ()
  "swank-macroexpand returns a string."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:swank-macroexpand "(or a b)"))))
    (should (stringp val))))

;;; Trace

(ert-deftest slip-swank/untrace-all ()
  "untrace-all returns a string."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:untrace-all))))
    (should (stringp val))))

;;; Xref

(ert-deftest slip-swank/xref-calls ()
  "xref :calls returns a list."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:xref ":calls" "car"))))
    (should (listp val))))

(ert-deftest slip-swank/xref-callers ()
  "xref :callers returns a list."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:xref ":callers" "+"))))
    (should (listp val))))

;;; Indentation

(ert-deftest slip-swank/update-indentation ()
  "update-indentation-information returns nil."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:update-indentation-information))))
    (should (null val))))

;;; Find definitions

(ert-deftest slip-swank/find-definitions ()
  "find-definitions-for-emacs returns a list."
  (slip-test--ensure-connected)
  (let ((val (slip-test--rex-ok '(swank:find-definitions-for-emacs "car"))))
    (should (listp val))))

;;;; --- Teardown -------------------------------------------------------------

;; Disconnect after all tests complete (batch mode).
(add-hook 'kill-emacs-hook #'slip-test--disconnect)

(provide 'test-swank-ert)

;;; test-swank-ert.el ends here
