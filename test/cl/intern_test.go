// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestInternNew(t *testing.T) {
	(&sliptest.Function{
		Source: `(intern "intern-test-new")`,
		Expect: "intern-test-new, nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(intern "intern-test-new")`,
		Expect: "intern-test-new, :internal",
	}).Test(t)
}

func TestInternInherited(t *testing.T) {
	(&sliptest.Function{
		Source: `(intern "*package*")`,
		Expect: "*package*, :inherited",
	}).Test(t)
	(&sliptest.Function{
		Source: `(intern "*package*" *common-lisp*)`,
		Expect: "*package*, :internal",
	}).Test(t)
}

func TestInternExternal(t *testing.T) {
	(&sliptest.Function{
		Source: `(intern "xyz" 'keyword)`,
		Expect: ":xyz, nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(intern "xyz" "keyword")`,
		Expect: ":xyz, :external",
	}).Test(t)
}

func TestInternArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(intern "xyz" 'keyword t)`,
		Panics: true,
	}).Test(t)
}

func TestInternNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(intern t)`,
		Panics: true,
	}).Test(t)
}

func TestInternNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source: `(intern "xyz" t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(intern "xyz" "not-a-package")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(intern "xyz" 'not-a-package)`,
		Panics: true,
	}).Test(t)
}
