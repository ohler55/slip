// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWithZipReaderOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((src (base64-decode "H4sIEAAAAAAA/3Rlc3QAKs7PTVVISSxJBAAAAP//AQAA//8e6cLZCQAAAA==")))
                   (with-input-from-octets (s src)
                     (with-zip-reader (z s)
                       (read-all z))))`,
		Expect: `"some data"`,
	}).Test(t)
}

func TestWithZipReaderArgsNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-zip-reader t nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithZipReaderNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-zip-reader (5 nil) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithZipReaderNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-zip-reader (z t) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithZipReaderError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-input-from-octets (s "xxxxx") (with-zip-reader (z s) nil))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
