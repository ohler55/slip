// Copyright (c) 2023, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDescribeMethodPlain(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*print-ansi*"), nil)
	_ = slip.ReadString("(describe-method vanilla-flavor :id out)", scope).Eval(scope, nil)
	tt.Equal(t, `:id is a method of vanilla-flavor
  Lambda-List: ()
  Return: fixnum
  Documentation:
    Returns the identifier of the instance.

  Implemented by:
    vanilla-flavor :primary
`, compactEmptyLines(out.String()))
}

func TestDescribeMethodAnsi(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*print-ansi*"), slip.True)
	_ = slip.ReadString("(describe-method vanilla-flavor :id out)", scope).Eval(scope, nil)
	tt.Equal(t, "\x1b[1m:id\x1b[m is a method of \x1b[1mvanilla-flavor\x1b[m\n"+
		"  Lambda-List: ()\n"+
		"  Return: fixnum\n"+
		"  Documentation:\n"+
		"    Returns the identifier of the instance.\n"+
		"\n"+
		"  Implemented by:\n"+
		"    \x1b[1mvanilla-flavor\x1b[m :primary\n",
		compactEmptyLines(out.String()))
}

func TestDescribeMethodDaemons(t *testing.T) {
	defer undefFlavors("berry", "blueberry")
	defineBerry(t)
	defineBlueberry(t)

	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*print-ansi*"), nil)
	_ = slip.ReadString(`(defwhopper (berry :rot) () "Does nothing." (continue-whopper))`, scope).Eval(scope, nil)
	_ = slip.ReadString("(describe-method 'blueberry :rot out)", scope).Eval(scope, nil)
	tt.Equal(t, `:rot is a method of blueberry
  Lambda-List: ()
  Documentation:
    When blueberries rot they turn mushy.

  Implemented by:
    berry :whopper
    blueberry :before
    berry :primary
    berry :after
    blueberry :after
`, compactEmptyLines(out.String()))
}

func TestDescribeMethodBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(describe-method 'bad)`,
		Panics: true,
	}).Test(t)
}

func TestDescribeMethodNotFlavor(t *testing.T) {
	(&sliptest.Function{
		Source:    `(describe-method t :id)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(describe-method 'not-a-flavor :id)`,
		PanicType: slip.ClassNotFoundSymbol,
	}).Test(t)
}

func TestDescribeMethodNotMethod(t *testing.T) {
	(&sliptest.Function{
		Source:    `(describe-method 'vanilla-flavor :not-a-method)`,
		PanicType: slip.UnboundSlotSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(describe-method 'fixnum :not-a-method)`,
		PanicType: slip.UnboundSlotSymbol,
	}).Test(t)
}

func TestDescribeMethodNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(describe-method 'vanilla-flavor :id t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
