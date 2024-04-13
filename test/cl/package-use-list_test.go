// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"fmt"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPackageUseList(t *testing.T) {
	(&sliptest.Function{
		Source: `(package-use-list *common-lisp-user*)`,
		Validate: func(t *testing.T, v slip.Object) {
			str := slip.ObjectString(v)
			for _, name := range []string{
				"keyword",
				"common-lisp",
				"xml",
				"net",
				"gi",
				"watch",
				"csv",
				"bag",
				"flavors",
				"clos",
				"test",
			} {
				tt.Equal(t, fmt.Sprintf("/#<package %s>/", name), str)
			}
		},
	}).Test(t)
}

func TestPackageUseListNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source:    `(package-use-list t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
