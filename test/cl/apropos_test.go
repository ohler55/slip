// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestAproposString(t *testing.T) {
	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	var out strings.Builder
	slip.StandardOutput = &slip.OutputStream{Writer: &out}

	(&sliptest.Function{
		Source: `(apropos "terpr")`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, "terpri (built-in)\n", out.String())
}

func TestAproposSymbol(t *testing.T) {
	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	var out strings.Builder
	slip.StandardOutput = &slip.OutputStream{Writer: &out}

	(&sliptest.Function{
		Source: `(apropos 'lambda)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, `/.*print-lambda. = nil
.*lambda \(built-in\)
.*/`, out.String())
}

func TestAproposPkg(t *testing.T) {
	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	var out strings.Builder
	slip.StandardOutput = &slip.OutputStream{Writer: &out}

	(&sliptest.Function{
		Source: `(apropos 'deff 'flavors)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, `flavors::defflavor (built-in)
flavors::undefflavor (built-in)
`, out.String())
}

func TestAproposPkgVars(t *testing.T) {
	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	var out strings.Builder
	slip.StandardOutput = &slip.OutputStream{Writer: &out}

	(&sliptest.Function{
		Source: `(apropos 'vanilla 'flavors)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, `flavors::vanilla-flavor = #<flavor vanilla-flavor>
`, out.String())
}

func TestAproposDefun(t *testing.T) {
	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	var out strings.Builder
	slip.StandardOutput = &slip.OutputStream{Writer: &out}

	_ = slip.CompileString("(defun apropos-test () nil)")

	(&sliptest.Function{
		Source: `(apropos 'test 'user)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, `common-lisp-user::apropos-test (lambda)
`, out.String())
}

func TestAproposBadArg(t *testing.T) {
	(&sliptest.Function{
		Source: `(apropos)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(apropos t)`,
		Panics: true,
	}).Test(t)
}

func TestAproposBadPkg(t *testing.T) {
	(&sliptest.Function{
		Source: `(apropos 'def "boo")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(apropos 'def t)`,
		Panics: true,
	}).Test(t)
}

func TestAproposWriteFail(t *testing.T) {
	orig := slip.StandardOutput
	defer func() { slip.StandardOutput = orig }()
	slip.StandardOutput = &slip.OutputStream{Writer: badWriter(0)}

	(&sliptest.Function{
		Source: `(apropos 'def)`,
		Panics: true,
	}).Test(t)
}
