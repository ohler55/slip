// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
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
                             (send cmd :stdout)
                             (send cmd :set-stdout nil)
                             (send cmd :stdout))))
                   result)`,
		Expect: `(nil #<OUTPUT-STREAM> #<OUTPUT-STREAM> nil nil)`,
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
                             (send cmd :stderr)
                             (send cmd :set-stderr nil)
                             (send cmd :stderr))))
                   result)`,
		Expect: `(nil #<OUTPUT-STREAM> #<OUTPUT-STREAM> nil nil)`,
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
                             (send cmd :stdin)
                             (send cmd :set-stdin nil)
                             (send cmd :stdin))))
                   result)`,
		Expect: `(nil #<INPUT-STREAM> #<INPUT-STREAM> nil nil)`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-instance 'command :path "echo") :set-stdin t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestCommandRunOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((cmd (make-instance 'command :path "sleep" :args '("0.01")))
                       p)
                   (send cmd :run)
                   ;; process completed so pid and process should exist
                   (list
                    (send cmd :pid)
                    (setq p (send cmd :process))
                    (when p (send p :exit-code))))`,
		Validate: func(t *testing.T, v slip.Object) {
			list, _ := v.(slip.List)
			tt.Equal(t, 3, len(list))
			tt.SameType(t, slip.Fixnum(0), list[0])
			tt.NotNil(t, list[1])
			tt.Equal(t, slip.Fixnum(0), list[2])
		},
	}).Test(t)
}

func TestCommandRunPanics(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((cmd (make-instance 'command :path "sleep" :args '("0.01")))
                       p)
                   (send cmd :run)
                   (list
                     (recover r 'denied (send cmd :set-path "echo"))
                     (recover r 'denied (send cmd :set-args '()))
                     (recover r 'denied (send cmd :set-dir "."))
                     (recover r 'denied (send cmd :set-env '()))
                     (recover r 'denied (send cmd :set-stdout nil))
                     (recover r 'denied (send cmd :set-stderr nil))
                     (recover r 'denied (send cmd :set-stdin nil))
                     (recover r 'denied (send cmd :run))
                   ))`,
		Expect: "(denied denied denied denied denied denied denied denied)",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-instance 'command :path "sleep" :args '("-1")) :run)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestCommandStart(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((cmd (make-instance 'command :path "sleep" :args '("1")))
                       p result)
                   (send cmd :start)
                   (addf result (send cmd :pid))
                   (addf result (recover r 'denied (send cmd :start)))
                   (setq p (send cmd :process))
                   (addf result (send p :exit-code))
                   (send p :kill)
                   (send p :wait)
                   result)`,
		Validate: func(t *testing.T, v slip.Object) {
			list, _ := v.(slip.List)
			tt.Equal(t, 3, len(list))
			tt.SameType(t, slip.Fixnum(0), list[0])
			tt.Equal(t, slip.Symbol("denied"), list[1])
			tt.Equal(t, slip.Fixnum(-1), list[2])
		},
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send (make-instance 'command :path "quux" :args '("-1")) :start)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
