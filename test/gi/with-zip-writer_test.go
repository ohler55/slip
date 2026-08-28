// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWithZipWriter5(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode
                  (with-output-to-string (s)
                    (with-zip-writer (z s 5 :comment "test")
                      (format z "some data"))))`,
		Validate: func(t *testing.T, v slip.Object) {
			checkZipB64(t, v, "test", "some data")
		},
	}).Test(t)
}

func TestWithZipWriterPreProv(t *testing.T) {
	orig := slip.Provenance
	slip.Provenance = true
	defer func() { slip.Provenance = orig }()
	(&sliptest.Function{
		Source: `(base64-encode
                  (with-output-to-string (s)
                    (with-zip-writer (z s 5 :comment "test")
                      (format z "some data"))))`,
		Validate: func(t *testing.T, v slip.Object) {
			checkZipB64(t, v, "test", "some data")
		},
	}).Test(t)
}

func TestWithZipWriterDefault(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode
                  (with-output-to-string (s)
                    (with-zip-writer (z s)
                      (format z "some data"))))`,
		Validate: func(t *testing.T, v slip.Object) {
			checkZipB64(t, v, "", "some data")
		},
	}).Test(t)
}

func TestWithZipWriterNilLevel(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode
                  (with-output-to-string (s)
                    (with-zip-writer (z s nil)
                      (format z "some data"))))`,
		Validate: func(t *testing.T, v slip.Object) {
			checkZipB64(t, v, "", "some data")
		},
	}).Test(t)
}

func TestWithZipWriterArgsNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-zip-writer t nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithZipWriterNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-zip-writer (5 nil) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithZipWriterNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-zip-writer (z t) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithZipWriterBadLevel(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-output-to-string (s) (with-zip-writer (z s t) nil))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithZipWriterError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-output-to-string (s) (with-zip-writer (z s 20) nil))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
