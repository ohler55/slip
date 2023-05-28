// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"bytes"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestGetOutputStreamStringOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((sos (make-string-output-stream)))
                  (princ "abc" sos)
                  (get-output-stream-string sos))`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestGetOutputStreamStringNotStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(get-output-stream-string t)`,
		Panics: true,
	}).Test(t)
}

func TestGetOutputStreamStringNotStringStream(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(get-output-stream-string out)`,
		Panics: true,
	}).Test(t)
}
