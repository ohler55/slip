// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakePackageSimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-package 'pack-test-1 :nicknames '(pt1))`,
		Validate: func(t *testing.T, v slip.Object) {
			tt.Equal(t, "#<package pack-test-1>", slip.ObjectString(v))
			p := v.(*slip.Package)
			tt.Equal(t, "[pt1]", pretty.SEN(p.Nicknames))
		},
	}).Test(t)
}

func TestMakePackageUse(t *testing.T) {
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'pack-test-2 :nicknames '(pt2))
                  (make-package 'pack-test-3 :nicknames '(pt3))
                  (make-package 'pack-test-4 :nicknames '(pt4))
                  (make-package 'pack-test-5 :use (list 'pt2 "pt3" (find-package 'pt4))))`,
		Validate: func(t *testing.T, v slip.Object) {
			tt.Equal(t, "#<package pack-test-5>", slip.ObjectString(v))
			p5 := v.(*slip.Package)
			p2 := slip.FindPackage("pt2")
			tt.Equal(t, "#<package pack-test-2>", slip.ObjectString(p2))

			tt.Equal(t, 3, len(p5.Uses))
			tt.Equal(t, "#<package pack-test-2>", slip.ObjectString(p5.Uses[0]))
			tt.Equal(t, "#<package pack-test-3>", slip.ObjectString(p5.Uses[1]))
			tt.Equal(t, "#<package pack-test-4>", slip.ObjectString(p5.Uses[2]))
			tt.Equal(t, 1, len(p2.Users))
			tt.Equal(t, "#<package pack-test-5>", slip.ObjectString(p2.Users[0]))
		},
	}).Test(t)
}

func TestMakePackageExists(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-package 'bag)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-package 'pack-test-6 :nicknames '(bag))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestMakePackageBadNicknames(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-package 'test-pack-6 :nicknames t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMakePackageBadUse(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-package 'test-pack-6 :use t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-package 'test-pack-6 :use '(t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestMakePackageUseNotFound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-package 'test-pack-6 :use '(quux))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
