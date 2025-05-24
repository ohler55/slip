// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestZip5(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (zip "some data" 5))`,
		Expect: `"H4sIAAAAAAAA/yrOz01VSEksSQQAAAD//wEAAP//HunC2QkAAAA="`,
	}).Test(t)
}

func TestZipDefault(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (zip "some data" nil))`,
		Expect: `"H4sIAAAAAAAA/yrOz01VSEksSQQAAAD//wEAAP//HunC2QkAAAA="`,
	}).Test(t)
}

func TestZipHeader(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (zip "some data" 5 :comment "test" :extra "xxx" :name "namai" :os 7))`,
		Expect: `"H4sIHAAAAAAABwMAeHh4bmFtYWkAdGVzdAAqzs9NVUhJLEkEAAAA//8BAAD//x7pwtkJAAAA"`,
	}).Test(t)
}

func TestZipModTime(t *testing.T) {
	(&sliptest.Function{
		Source: `(base64-encode (zip "some data" 9 :mod-time @2025-05-23T12:13:14Z))`,
		Expect: `"H4sIAFpmMGgC/yrOz01VSEksSQQAAAD//wEAAP//HunC2QkAAAA="`,
	}).Test(t)
}

func TestZipBadLevel(t *testing.T) {
	(&sliptest.Function{
		Source:    `(zip "some data" t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestZipBadTime(t *testing.T) {
	(&sliptest.Function{
		Source:    `(zip "some data" :mod-time t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestZipNewWriterError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(zip "some data" 100)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestZipError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(zip "some data" :comment "ぴ")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
