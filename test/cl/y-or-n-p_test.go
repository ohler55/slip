// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestYOrNPY(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*standard-input* (make-string-input-stream "y\n"))
                       (*standard-output* (make-string-output-stream)))
                  (list
                   (y-or-n-p "Will the test ~A" 'pass)
                   (get-output-stream-string *standard-output*)))`,
		Expect: "(t \"Will the test pass (y or n) \n\")",
	}).Test(t)
}

func TestYOrNPN(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*standard-input* (make-string-input-stream "n\n"))
                       (*standard-output* (make-string-output-stream)))
                  (list
                   (y-or-n-p "Will the test ~A" 'pass)
                   (get-output-stream-string *standard-output*)))`,
		Expect: "(nil \"Will the test pass (y or n) \n\")",
	}).Test(t)
}

func TestYOrNPZY(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*standard-input* (make-string-input-stream "z\ny\n"))
                       (*standard-output* (make-string-output-stream)))
                  (list
                   (y-or-n-p "Will the test ~A" 'pass)
                   (get-output-stream-string *standard-output*)))`,
		Expect: "(t\n" +
			" \"Will the test pass (y or n) Please type \"y\" for yes or \"n\" for no.\n" +
			"Will the test pass (y or n) \n\")",
	}).Test(t)
}

func TestYOrNPBadWrite(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("bad"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((*standard-input* (make-string-input-stream "z\ny\n"))
                       (*standard-output* bad))
                  (y-or-n-p))`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}

func TestYOrNPBadRead(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("bad"), &slip.InputStream{Reader: badReader(0)})
	(&sliptest.Function{
		Scope: scope,
		Source: `(let ((*standard-input* bad)
                       (*standard-output* (make-string-output-stream)))
                  (y-or-n-p))`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}
