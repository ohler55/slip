// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFileLengthOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-open-file (file "testdata/map.sen" :direction :input) (file-length file))`,
		Expect: "18",
	}).Test(t)
}

func TestFileLengthNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-length t)`,
		Panics: true,
	}).Test(t)
}
