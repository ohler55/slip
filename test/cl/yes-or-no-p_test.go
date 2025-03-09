// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestYesOrNoPY(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*standard-input* (make-string-input-stream "yes\n"))
                       (*standard-output* (make-string-output-stream)))
                  (list
                   (yes-or-no-p "Will the test ~A" 'pass)
                   (get-output-stream-string *standard-output*)))`,
		Expect: "(t \"Will the test pass (yes or no) \n\")",
	}).Test(t)
}

func TestYesOrNoPN(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*standard-input* (make-string-input-stream "no\n"))
                       (*standard-output* (make-string-output-stream)))
                  (list
                   (yes-or-no-p "Will the test ~A" 'pass)
                   (get-output-stream-string *standard-output*)))`,
		Expect: "(nil \"Will the test pass (yes or no) \n\")",
	}).Test(t)
}

func TestYesOrNoPZY(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*standard-input* (make-string-input-stream "z\nyes\n"))
                       (*standard-output* (make-string-output-stream)))
                  (list
                   (yes-or-no-p "Will the test ~A" 'pass)
                   (get-output-stream-string *standard-output*)))`,
		Expect: "(t\n" +
			" \"Will the test pass (yes or no) Please type \"yes\" for yes or \"no\" for no.\n" +
			"Will the test pass (yes or no) \n\")",
	}).Test(t)
}

func TestYesOrNoPBadWrite(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("bad"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((*standard-input* (make-string-input-stream "z\ny\n"))
                       (*standard-output* bad))
                  (yes-or-no-p))`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}

func TestYesOrNoPBadRead(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("bad"), &slip.InputStream{Reader: badReader(0)})
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((*standard-input* bad)
                       (*standard-output* (make-string-output-stream)))
                  (yes-or-no-p))`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}
