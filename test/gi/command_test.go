// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCommandInit(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((cmd (make-instance 'command :path "echo" :args '("hello" "world") :dir ".")))
                   (list
                    (send cmd :pid)
                    (send cmd :path)
                    (send cmd :args)
                    (send cmd :dir)
                    cmd))`,
		Expect: `/\(nil ".*echo" \("echo" "hello" "world"\) "." #<command [0-9a-f]+>\)/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((cmd (make-instance 'command :path "echo" :args '("hello" "world"))))
                   (send cmd :set-path "sleep")
                   (send cmd :set-args '("sleep" "1"))
                   (send cmd :set-dir "testdata")
                   (list
                    (send cmd :path)
                    (send cmd :args)
                    (send cmd :dir)
                    cmd))`,
		Expect: `/\(".*sleep" \("sleep" "1"\) "testdata" #<command [0-9a-f]+>\)/`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-instance 'command :path "echo" :args t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-instance 'command :path "echo") :set-args t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestCommandEnv(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((cmd (make-instance 'command :path "echo" :args '("hello"))))
                   (list
                    (send cmd :env)
                    (send cmd :set-env '("QUUX=quux"))
                    (send cmd :env)))`,
		Expect: `(() ("QUUX=quux") ("QUUX=quux"))`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-instance 'command :path "echo") :set-env t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestCommandStdout(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((cmd (make-instance 'command :path "echo" :args '("hello")))
                       result)
                   (with-output-to-string (s)
                     (setq result
                           (list
                             (send cmd :stdout)
                             (send cmd :set-stdout s)
                             (send cmd :stdout))))
                   result)`,
		Expect: `(nil #<OUTPUT-STREAM> #<OUTPUT-STREAM>)`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-instance 'command :path "echo") :set-stdout t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestCommandStderr(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((cmd (make-instance 'command :path "echo" :args '("hello")))
                       result)
                   (with-output-to-string (s)
                     (setq result
                           (list
                             (send cmd :stderr)
                             (send cmd :set-stderr s)
                             (send cmd :stderr))))
                   result)`,
		Expect: `(nil #<OUTPUT-STREAM> #<OUTPUT-STREAM>)`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-instance 'command :path "echo") :set-stderr t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestCommandStdin(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((cmd (make-instance 'command :path "echo" :args '("hello")))
                       result)
                   (with-input-from-string (s "quux")
                     (setq result
                           (list
                             (send cmd :stdin)
                             (send cmd :set-stdin s)
                             (send cmd :stdin))))
                   result)`,
		Expect: `(nil #<INPUT-STREAM> #<INPUT-STREAM>)`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-instance 'command :path "echo") :set-stdin t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestCommandRun(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((cmd (make-instance 'command :path "sleep" :args '("0.01"))))
                   ;; process completed so pid and process should exist
                   (list
                    (send cmd :pid)
                    (send cmd :process)))`,
		Expect: `/xx/`,
	}).Test(t)
}
