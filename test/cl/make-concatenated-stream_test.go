// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeConcatenatedStreamOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((ss1 (make-string-input-stream "abc"))
                        (ss2 (make-string-input-stream "def"))
                        (cs (make-concatenated-stream ss1 ss2)))
                  (read cs))`,
		Expect: `abcdef`,
	}).Test(t)
}

func TestMakeConcatenatedStreamStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(streamp (make-concatenated-stream (make-string-input-stream "x")))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeConcatenatedStreaminputStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(input-stream-p (make-concatenated-stream (make-string-input-stream "x")))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeConcatenatedStreamOutputStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(output-stream-p (make-concatenated-stream (make-string-input-stream "x")))`,
		Expect: "nil",
	}).Test(t)
}

func TestMakeConcatenatedStreamOpenStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(open-stream-p (make-concatenated-stream (make-string-input-stream "x")))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(open-stream-p (make-concatenated-stream))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeConcatenatedStreamNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-concatenated-stream t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
