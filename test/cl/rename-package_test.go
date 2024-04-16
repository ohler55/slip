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
	defer func() {
		slip.RemovePackage(slip.FindPackage("rename-test-1"))
		slip.RemovePackage(slip.FindPackage("rename-test-2"))
	}()
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
	defer func() {
		slip.RemovePackage(slip.FindPackage("rename-test-1"))
		slip.RemovePackage(slip.FindPackage("rename-test-2"))
	}()
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'rename-test-1)
                  (rename-package 'rename-test-1 'rename-test-2))`,
		Expect: "#<package rename-test-2>",
	}).Test(t)
}

func TestRenamePackageString(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("rename-test-1"))
		slip.RemovePackage(slip.FindPackage("rename-test-2"))
	}()
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'rename-test-1)
                  (rename-package "rename-test-1" "rename-test-2"))`,
		Expect: "#<package rename-test-2>",
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
		PanicType: slip.PackageErrorSymbol,
	}).Test(t)
}

func TestRenamePackageExists(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("rename-test-1"))
	}()
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'rename-test-1)
                  (rename-package "rename-test-1" "bag"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestRenamePackageBadNicknames(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("rename-test-1"))
		slip.RemovePackage(slip.FindPackage("rename-test-2"))
	}()
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'rename-test-1)
                  (rename-package "rename-test-1" "rename-test-2" t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestRenamePackageBadNickname(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("rename-test-1"))
		slip.RemovePackage(slip.FindPackage("rename-test-2"))
	}()
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'rename-test-1)
                  (rename-package "rename-test-1" "rename-test-2" '(bag)))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
