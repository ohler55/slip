// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func xTestDefwhopperBasic(t *testing.T) {
	defer undefFlavor("berry")
	defineBerry(t)
	scope := slip.NewScope()

	var b strings.Builder
	scope.Set(slip.Symbol("out"), &slip.OutputStream{Writer: &b})

	_ = slip.ReadString(`
(defwhopper (berry :rot) ()
 (princ "berry whopper rot") (terpri)
 (continue-whopper))
`).Eval(scope)

	f := slip.ReadString("berry").Eval(scope)
	sf := f.Simplify()
	tt.Equal(t, true, jp.MustParseString("methods[*][?(@.name == ':rot')].whopper").First(sf))

	_ = slip.ReadString("(setq bb (make-instance berry))").Eval(scope)
	_ = slip.ReadString("(send bb :rot)").Eval(scope)
	tt.Equal(t, `
berry rot
blueberry after rot
berry after rot
`, b.String())

}
