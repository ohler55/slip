// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestListAllPackagesOk(t *testing.T) {
	p := slip.DefaultPrinter()
	orig := p.Pretty
	defer func() { p.Pretty = orig }()
	p.Pretty = false
	// Turn off pretty so regexp doesn't fail on the newline.
	(&sliptest.Function{
		Source: `(list-all-packages)`,
		Expect: `/^\(#<package bag> .*#<package common-lisp> #<package common-lisp-user> ` +
			`.*#<package flavors>.*#<package gi>.*#<package keyword>.*#<package test>\)$/`,
	}).Test(t)
}

func TestListAllPackagesBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(list-all-packages t)`,
		Panics: true,
	}).Test(t)
}
