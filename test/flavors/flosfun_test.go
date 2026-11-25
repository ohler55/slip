// Copyright (c) 2024, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestFlosfunDocFlavor(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`(flosfun 'foo-foo :flavor vanilla-flavor)`, scope)

	result := slip.ReadEvalString("(foo-foo (make-instance 'vanilla-flavor))", scope)
	tt.Equal(t, "#<flavor vanilla-flavor>", slip.ObjectString(result))

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*print-ansi*"), nil)
	_ = slip.ReadEvalString("(describe 'foo-foo out)", scope)

	tt.Equal(t, "/foo-foo names/", out.String())
	tt.Equal(t, "/Return: flavor/", out.String()) // pulled in from vanilla-flavor
}

func TestFlosfunDocString(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`(flosfun "foo-zoo" :flavor ":flavor.")`, scope)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*print-ansi*"), nil)
	_ = slip.ReadEvalString("(describe 'foo-zoo out)", scope)

	tt.Equal(t, "/foo-zoo sends/", out.String())
}

func TestFlosfunDocsNil(t *testing.T) {
	scope := slip.NewScope()
	defer func() {
		_ = slip.ReadEvalString(`(fmakunbound 'floss-package:flossy)`, scope)
		slip.RemovePackage(slip.FindPackage("floss-package"))
	}()
	pkg := slip.DefPackage("floss-package", nil, "")

	flavors.FlosFun("flossy", ":floss", nil, pkg)

	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*print-ansi*"), nil)
	_ = slip.ReadEvalString("(describe 'floss-package:flossy out)", scope)

	tt.Equal(t, "/flossy names a flos-function/", out.String())
	tt.Equal(t, "/The instance to send the :floss method to/", out.String())
}

func TestFlosfunNotInstance(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`(flosfun "foo-boo" :flavor ":flavor.")`, scope)

	(&sliptest.Function{
		Source:    `(foo-boo t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestFlosfunBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(flosfun t :flavor)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestFlosfunBadMethod(t *testing.T) {
	(&sliptest.Function{
		Source:    `(flosfun "foo-bar" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestFlosfunNotMethod(t *testing.T) {
	(&sliptest.Function{
		Source:    `(flosfun "foo-bar" :nothing vanilla-flavor)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestFlosfunNotDocSource(t *testing.T) {
	(&sliptest.Function{
		Source:    `(flosfun "foo-bar" :nothing t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
