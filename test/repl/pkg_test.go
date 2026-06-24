// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReplEditorFlags(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((orig *repl-editor-flags*)
                       result)
                   (setq *repl-editor-flags* nil)
                   (addf result *repl-editor-flags*)
                   (setq *repl-editor-flags* '("-v"))
                   (addf result *repl-editor-flags*)
                   (setq *repl-editor-flags* orig)
                   result)`,
		Expect: `(() ("-v"))`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setq *repl-editor-flags* 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReplExternalEditor(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((orig *repl-external-editor*)
                       result)
                   (setq *repl-external-editor* "emacs")
                   (setq result *repl-external-editor*)
                   (setq *repl-external-editor* orig)
                   result)`,
		Expect: `"emacs"`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setq *repl-external-editor* 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReplEvalOnClose(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((orig *repl-eval-on-close*)
                       result)
                   (setq *repl-eval-on-close* t)
                   (addf result *repl-eval-on-close*)
                   (setq *repl-eval-on-close* nil)
                   (addf result *repl-eval-on-close*)
                   (setq *repl-eval-on-close* orig)
                   result)`,
		Expect: `(t nil)`,
	}).Test(t)
}

func TestReplInteractive(t *testing.T) {
	(&sliptest.Function{
		Source: `*repl-interactive*`,
		Validate: func(t *testing.T, v slip.Object) {
			// If no panic then pass
		},
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setq *repl-interactive* t)`,
		PanicType: slip.PackageErrorSymbol,
	}).Test(t)
}

func TestReplStashLoadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((orig *stash-load-path*)
                       result)
                   (setq *stash-load-path* nil)
                   (addf result *stash-load-path*)
                   (setq *stash-load-path* '("."))
                   (addf result *stash-load-path*)
                   (setq *stash-load-path* orig)
                   result)`,
		Expect: `(() ("."))`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setq *stash-load-path* 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReplDefaultStashName(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((orig *default-stash-name*)
                       result)
                   (setq *default-stash-name* nil)
                   (addf result *default-stash-name*)
                   (setq *default-stash-name* "quux.lisp")
                   (addf result *default-stash-name*)
                   (setq *default-stash-name* 'sym)
                   (addf result *default-stash-name*)
                   (setq *default-stash-name* orig)
                   result)`,
		Expect: `(nil "quux.lisp" "sym")`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setq *default-stash-name* 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
