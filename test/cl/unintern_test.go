// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUninternNew(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(defvar unintern-test-new 7)").Eval(scope)
	(&sliptest.Function{
		Source: `(unintern 'unintern-test-new)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(unintern 'unintern-test-new)`,
		Expect: "nil",
	}).Test(t)
}

func TestUninternInherited(t *testing.T) {
	(&sliptest.Function{
		Source: `(unintern '*package*)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(unintern '*package* *common-lisp*)`,
		Panics: true,
	}).Test(t)
}

func TestUninternExternal(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(intern "unintern-test-external" 'keyword)`).Eval(scope)
	(&sliptest.Function{
		Source: `(unintern 'unintern-test-external 'keyword)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(unintern 'unintern-test-external "keyword")`,
		Expect: "nil",
	}).Test(t)
}

func TestUninternArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(unintern 'xyz 'keyword t)`,
		Panics: true,
	}).Test(t)
}

func TestUninternNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(unintern t)`,
		Panics: true,
	}).Test(t)
}

func TestUninternNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source: `(unintern 'xyz t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(unintern 'xyz "not-a-package")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(unintern 'xyz 'not-a-package)`,
		Panics: true,
	}).Test(t)
}
