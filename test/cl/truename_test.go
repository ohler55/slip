// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTruenameStringOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(truename "truename_test.go")`,
		Expect: `/.*\/slip\/test\/cl\/truename_test.go/`,
	}).Test(t)
}

func TestTruenameStringNoFile(t *testing.T) {
	(&sliptest.Function{
		Source:    `(truename "truename_test.lisp")`,
		PanicType: slip.FileErrorSymbol,
	}).Test(t)
}

func TestTruenameNotFilespec(t *testing.T) {
	(&sliptest.Function{
		Source:    `(truename t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestTruenameFileStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-open-file (f "truename_test.go" :direction :input) (truename f))`,
		Expect: `/.*\/slip\/test\/cl\/truename_test.go/`,
	}).Test(t)
}

func TestTruenameSynonymStreamOk(t *testing.T) {
	slip.CurrentPackage.Set("zz", nil)
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()

	(&sliptest.Function{
		Source: `(with-open-file (f "truename_test.go" :direction :input)
                  (let ((ss (make-synonym-stream 'zz)))
                   (setq zz f)
                   (truename ss)))`,
		Expect: `/.*\/slip\/test\/cl\/truename_test.go/`,
	}).Test(t)
}

func TestTruenameSynonymStreamUnbound(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((ss (make-synonym-stream 'zz)))
                  (truename ss))`,
		PanicType: slip.UnboundVariableSymbol,
	}).Test(t)
}
