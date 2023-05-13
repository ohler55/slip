// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDirectoryOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(directory "testdata/*.lisp")`,
		Validate: func(t *testing.T, v slip.Object) {
			str := slip.ObjectString(v)
			tt.Equal(t, true, strings.Contains(str, "load-me-too.lisp"))
			tt.Equal(t, true, strings.Contains(str, "load-me.lisp"))
		},
	}).Test(t)
}

func TestDirectoryNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(directory t)`,
		Panics: true,
	}).Test(t)
}
