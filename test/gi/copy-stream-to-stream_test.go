// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCopyStreamToStreamOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-output-to-string (out)
                   (with-input-from-string (in "This is the data to copy.")
                     (copy-stream-to-stream in out)))`,
		Expect: `"This is the data to copy."`,
	}).Test(t)
}

func TestCopyStreamToStreamSmallBuffer(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-output-to-string (out)
                   (with-input-from-string (in "This is the data to copy.")
                     (copy-stream-to-stream in out :buffer-size 10)))`,
		Expect: `"This is the data to copy."`,
	}).Test(t)
}

func TestCopyStreamToStreamBadBufferSize(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-output-to-string (out)
                   (with-input-from-string (in "This is the data to copy.")
                     (copy-stream-to-stream in out :buffer-size 10.5)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestCopyStreamToStreamBadInput(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-output-to-string (out)
                   (copy-stream-to-stream t out))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestCopyStreamToStreamBadOutput(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-input-from-string (in "This is the data to copy.")
                   (copy-stream-to-stream in t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestCopyStreamToStreamReadError(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("in"), slip.NewInputStream(badReader(0)))
	(&sliptest.Function{
		Scope: scope,
		Source: `(with-output-to-string (out)
                   (copy-stream-to-stream in out))`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}

func TestCopyStreamToStreamWriteError(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope: scope,
		Source: `(with-input-from-string (in "This is the data to copy.")
                   (copy-stream-to-stream in out))`,
		PanicType: slip.StreamErrorSymbol,
	}).Test(t)
}
