// Copyright (c) 2023, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDescribeFlavor(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	scope.Let(slip.Symbol("*print-ansi*"), nil)
	_ = slip.ReadString("(describe-flavor vanilla-flavor out)").Eval(scope, nil)
	tt.Equal(t, `vanilla-flavor is a flavor:
  Documentation:
    A Flavor that implements the standard methods.
  Variables:
  Methods:
    :change-class
    :change-flavor
    :describe
    :equal
    :eval-inside-yourself
    :flavor
    :id
    :init
    :inspect
    :operation-handled-p
    :print-self
    :send-if-handles
    :shared-initialize
    :update-instance-for-different-class
    :which-operations
`, out.String())
}

func TestDescribeFlavorBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(describe-flavor)`,
		Panics: true,
	}).Test(t)
}

func TestDescribeFlavorNotFlavor(t *testing.T) {
	(&sliptest.Function{
		Source: `(describe-flavor t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(describe-flavor 'not-a-flavor)`,
		Panics: true,
	}).Test(t)
}

func TestDescribeFlavorNotStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(describe-flavor 'vanilla-flavor t)`,
		Panics: true,
	}).Test(t)
}
