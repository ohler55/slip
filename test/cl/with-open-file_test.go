// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestWithOpenFileBasic(t *testing.T) {
	tf := sliptest.Function{
		Source: `(with-open-file (file "testdata/map.sen" :direction :input) (make-instance bag-flavor :read file))`,
		Expect: "/#<bag-flavor .+>/",
	}
	tf.Test(t)
	inst, ok := tf.Result.(*flavors.Instance)
	tt.Equal(t, true, ok)
	result := inst.Receive(":write", nil, 0)
	tt.Equal(t, `"{a: 1 b: 2}"`, slip.ObjectString(result))
}

func TestWithOpenFileBadSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-open-file (t "testdata/map.sen" :direction :input) nil)`,
		Panics: true,
	}).Test(t)
}

func TestWithOpenFileNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-open-file t nil)`,
		Panics: true,
	}).Test(t)
}
