// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeBroadcastStreamOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((ss1 (make-string-output-stream))
                        (ss2 (make-string-output-stream))
                        (bs (make-broadcast-stream ss1 ss2)))
                  (princ 'hello bs)
                  (list
                   (get-output-stream-string ss1)
                   (get-output-stream-string ss2)
                   (file-length bs)
                   (file-position bs)))`,
		Expect: `("hello" "hello" nil 5)`,
	}).Test(t)
}

func TestMakeBroadcastStreamStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(streamp (make-broadcast-stream (make-string-output-stream)))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeBroadcastStreaminputStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(input-stream-p (make-broadcast-stream (make-string-output-stream)))`,
		Expect: "nil",
	}).Test(t)
}

func TestMakeBroadcastStreamOutputStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(output-stream-p (make-broadcast-stream (make-string-output-stream)))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeBroadcastStreamOpenStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(open-stream-p (make-broadcast-stream (make-string-output-stream)))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(open-stream-p (make-broadcast-stream))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeBroadcastStreamNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-broadcast-stream t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
