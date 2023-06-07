// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestDescribeBasic(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	scope.Let("*print-right-margin*", slip.Fixnum(80))

	expect := `common-lisp:car
  [symbol]

car names a built-in:
  Lambda-List: (arg)
  Return: object
  Documentation:
    car returns the car if arg is a cons, the first element if arg is a list,
    and nil if arg is nil or an empty list.
  Arguments:
    arg: [list|cons]
      The value to take the first element of.

  Examples:
    (car nil) => nil
    (car '(a . b) => a
    (car '(a b c)) => a

`
	(&sliptest.Function{
		Scope:  scope,
		Source: `(describe 'cl:car)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, expect, out.String())

	out.Reset()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(describe 'car)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, expect, out.String())
}

func TestDescribeNil(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", slip.True)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(describe nil)`,
		Expect: "",
	}).Test(t)

	tt.Equal(t, "\x1b[1mnil\x1b[m\n  [null]\n", out.String())
}

func TestDescribeVar(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", slip.True)
	scope.Let("sample", slip.Fixnum(7))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(describe 'sample)`,
		Expect: "",
	}).Test(t)

	tt.Equal(t,
		"\x1b[1mcommon-lisp-user:sample\x1b[m\n  [symbol]\n\n\x1b[1msample\x1b[m names a fixnum:\n  Value = 7\n",
		out.String())
}

func TestDescribeBaseType(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(describe 'xyz)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, "common-lisp-user:xyz\n  [symbol]\n  unbound\n", out.String())
}

func TestDescribeBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(describe)`,
		Panics: true,
	}).Test(t)
}

func TestDescribeBadStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(describe 'car t)`,
		Panics: true,
	}).Test(t)
}

func TestDescribeNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source: `(describe 'zz:car)`,
		Panics: true,
	}).Test(t)
}

func TestDescribeWriteFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: badWriter(0)})
	scope.Let("sample", nil)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(describe 'sample)`,
		Panics: true,
	}).Test(t)
}

func TestAppendDescribe(t *testing.T) {
	slip.CurrentPackage.Set("*package-load-path*", nil)
	b := cl.AppendDescribe(nil, slip.Symbol("*package-load-path*"), slip.NewScope(), 2, 80, false)
	tt.Equal(t, `  common-lisp:*package-load-path*
    [symbol]

  *package-load-path* names a null:
    Documentation:
        package load paths
      Value = nil
`, string(b))
}
