// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFilePositionOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-open-file (file "testdata/map.sen" :direction :input) (file-position file 3) (file-position file))`,
		Expect: "3",
	}).Test(t)
}

func TestFilePositionNotStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-position t)`,
		Panics: true,
	}).Test(t)
}

func TestFilePositionNotFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-open-file (file "testdata/map.sen" :direction :input) (file-position file t))`,
		Panics: true,
	}).Test(t)
}
