// Copyright (c) 2023, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestDescribeMethodPlain(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*print-ansi*"), nil)
	_ = slip.ReadString("(describe-method vanilla-flavor :id out)").Eval(scope)
	tt.Equal(t, `:id is a method of vanilla-flavor
  vanilla-flavor :primary
    :id => string
    Returns the identifier of the instance.
`, out.String())
}

func TestDescribeMethodAnsi(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*print-ansi*"), slip.True)
	_ = slip.ReadString("(describe-method vanilla-flavor :id out)").Eval(scope)
	tt.Equal(t, "\x1b[1m:id\x1b[m is a method of \x1b[1mvanilla-flavor\x1b[m\n"+
		"  \x1b[1mvanilla-flavor\x1b[m :primary\n"+
		"    \x1b[1m:id\x1b[m => \x1b[4mstring\x1b[m\n"+
		"    Returns the identifier of the instance.\n",
		out.String())
}
