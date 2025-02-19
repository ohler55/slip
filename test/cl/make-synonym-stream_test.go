// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeSynonymStreamOk(t *testing.T) {
	slip.CurrentPackage.Set("zz", nil)
	defer func() { _ = slip.CurrentPackage.Remove("zz") }()

	(&sliptest.Function{
		Source: `(let* ((ss1 (make-string-input-stream "abc"))
                        (ss2 (make-string-output-stream))
                        (ss (make-synonym-stream 'zz)))
                  (setq zz ss1)
                  (list
                   (read ss)
                   (setq zz ss2)
                   (princ "def" ss)
                   (get-output-stream-string ss2)))`,
		Expect: `(abc #<STRING-STREAM> "def" "def")`,
	}).Test(t)
}

func TestMakeSynonymStreamStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(streamp (make-synonym-stream 'zz))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeSynonymStreamInputStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(input-stream-p (make-synonym-stream 'zz))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeSynonymStreamOutputStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(output-stream-p (make-synonym-stream 'zz))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeSynonymStreamNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-synonym-stream t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
