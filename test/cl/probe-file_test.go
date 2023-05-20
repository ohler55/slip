// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestProbeFileExists(t *testing.T) {
	(&sliptest.Function{
		Source: `(probe-file "testdata/load-me.lisp")`,
		Expect: `/".*testdata.load-me.lisp"/`,
	}).Test(t)
}

func TestProbeFileNotExist(t *testing.T) {
	(&sliptest.Function{
		Source: `(probe-file "testdata/not-me.lisp")`,
		Expect: "nil",
	}).Test(t)
}

func TestProbeFileNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(probe-file t)`,
		Panics: true,
	}).Test(t)
}
