// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRenamePackagePackage(t *testing.T) {
	(&sliptest.Function{
		Source: `(rename-package (make-package 'rename-test-1) 'rename-test-2 '(rt2))`,
		Validate: func(t *testing.T, v slip.Object) {
			tt.Equal(t, "#<package rename-test-2>", slip.ObjectString(v))
			p := v.(*slip.Package)
			tt.Equal(t, "[rt2]", pretty.SEN(p.Nicknames))
		},
	}).Test(t)
}

func TestRenamePackageSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'rename-test-3)
                  (rename-package 'rename-test-3 'rename-test-4))`,
		Expect: "#<package rename-test-4>",
	}).Test(t)
}

func TestRenamePackageString(t *testing.T) {
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'rename-test-5)
                  (rename-package "rename-test-5" "rename-test-6"))`,
		Expect: "#<package rename-test-6>",
	}).Test(t)
}

func TestRenamePackageNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source:    `(rename-package t "rename-test-7")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestRenamePackageNotFound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(rename-package 'not-found "rename-test-7")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestRenamePackageExists(t *testing.T) {
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'rename-test-8)
                  (rename-package "rename-test-8" "bag"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestRenamePackageBadNicknames(t *testing.T) {
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'rename-test-9)
                  (rename-package "rename-test-9" "rename-test-10" t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'rename-test-11)
                  (rename-package "rename-test-11" "rename-test-12" '(bag)))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
