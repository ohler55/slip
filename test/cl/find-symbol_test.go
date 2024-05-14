// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFindSymbolNew(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-symbol "find-symbol-test-new")`,
		Expect: "nil, nil",
	}).Test(t)
	scope := slip.NewScope()
	_ = slip.ReadString("(defvar find-symbol-test-new 7)").Eval(scope, nil)
	(&sliptest.Function{
		Source: `(find-symbol "find-symbol-test-new")`,
		Expect: "find-symbol-test-new, :internal",
	}).Test(t)
}

func TestFindSymbolInherited(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-symbol "*package*")`,
		Expect: "*package*, :inherited",
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-symbol "*package*" *common-lisp*)`,
		Expect: "*package*, :external",
	}).Test(t)
}

func TestFindSymbolExternal(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-symbol "find-symbol-test-external" 'keyword)`,
		Expect: "nil, nil",
	}).Test(t)
	scope := slip.NewScope()
	_ = slip.ReadString(`(intern "find-symbol-test-external" 'keyword)`).Eval(scope, nil)
	(&sliptest.Function{
		Source: `(find-symbol "find-symbol-test-external" "keyword")`,
		Expect: ":find-symbol-test-external, :external",
	}).Test(t)
}

func TestFindSymbolArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-symbol "xyz" 'keyword t)`,
		Panics: true,
	}).Test(t)
}

func TestFindSymbolNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-symbol t)`,
		Panics: true,
	}).Test(t)
}

func TestFindSymbolNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-symbol "xyz" t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-symbol "xyz" "not-a-package")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-symbol "xyz" 'not-a-package)`,
		Panics: true,
	}).Test(t)
}
