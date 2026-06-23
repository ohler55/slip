// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestInPackageString(t *testing.T) {
	scope := slip.NewScope()
	orig := scope.Get("*package*")
	defer scope.Set("*package*", orig)
	(&sliptest.Function{
		Source: `(in-package "User")`,
		Expect: `#<package common-lisp-user>`,
	}).Test(t)
	p := scope.Get("*package*")
	tt.Equal(t, "#<package common-lisp-user>", slip.ObjectString(p))
}

func TestInPackageSymbol(t *testing.T) {
	scope := slip.NewScope()
	orig := scope.Get("*package*")
	defer scope.Set("*package*", orig)
	(&sliptest.Function{
		Source: `(in-package 'user)`,
		Expect: `#<package common-lisp-user>`,
	}).Test(t)
	p := scope.Get("*package*")
	tt.Equal(t, "#<package common-lisp-user>", slip.ObjectString(p))
}

func TestInPackageArgCount(t *testing.T) {
	(&sliptest.Function{
		Source:    `(in-package)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestInPackageBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(in-package t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(in-package 'quux)`,
		PanicType: slip.PackageErrorSymbol,
	}).Test(t)
}

func TestInPackageCallInternal(t *testing.T) {
	scope := slip.NewScope()
	orig := scope.Get("*package*")
	slip.CurrentPackage.Remove("quuxly")
	defer func() {
		scope.Set("*package*", orig)
		slip.RemovePackage(slip.FindPackage("quux"))
		slip.CurrentPackage.Remove("quuxly")
	}()
	(&sliptest.Function{
		Source: `
(setq inner-called nil)
(defpackage :quux (:use :common-lisp) (:export 'quuxly))
(in-package :quux)
(defun inner () (setq inner-called t))
(defun quuxly () (inner))
(quuxly)
;;(export 'quuxly)
(in-package 'user)
(use-package 'quux)
(setq inner-called nil)
(quuxly)
;;(inner)
inner-called
`,
		Expect: `t`,
	}).Test(t)
}

func TestInPackageCallInternalExportLater(t *testing.T) {
	scope := slip.NewScope()
	orig := scope.Get("*package*")
	slip.CurrentPackage.Remove("quuxly")
	defer func() {
		scope.Set("*package*", orig)
		slip.RemovePackage(slip.FindPackage("quux"))
		slip.CurrentPackage.Remove("quuxly")
	}()
	(&sliptest.Function{
		Source: `
(setq inner-called nil)
(defpackage :quux (:use :common-lisp))
(in-package :quux)
(defun inner () (setq inner-called t))
(defun quuxly () (inner))
(quuxly)
(export 'quuxly)
(in-package 'user)
(use-package 'quux)
(setq inner-called nil)
(quuxly)
inner-called
`,
		Expect: `t`,
	}).Test(t)
}

func TestInPackageCallInternalNoExport(t *testing.T) {
	scope := slip.NewScope()
	orig := scope.Get("*package*")
	slip.CurrentPackage.Remove("quuxly")
	defer func() {
		scope.Set("*package*", orig)
		slip.RemovePackage(slip.FindPackage("quux"))
		slip.CurrentPackage.Remove("quuxly")
	}()
	(&sliptest.Function{
		Source: `
(setq inner-called nil)
(defpackage :quux (:use :common-lisp))
(in-package :quux)
(defun inner () (setq inner-called t))
(defun quuxly () (inner))
(in-package 'user)
(use-package 'quux)
(quuxly)
`,
		PanicType: slip.UndefinedFunctionSymbol,
	}).Test(t)
}
