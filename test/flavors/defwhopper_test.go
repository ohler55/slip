// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestDefwhopperBasic(t *testing.T) {
	defer undefFlavors("berry", "blueberry")
	var b strings.Builder
	scope := slip.NewScope()
	scope.Set(slip.Symbol("out"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry :rot) () (princ "berry rot" out) (terpri out))
(defmethod (berry :after :rot) () (princ "berry after rot" out) (terpri out))
(defwhopper (berry :rot) ()
 (princ "berry whopper rot start" out) (terpri out)
 (continue-whopper)
 (princ "berry whopper rot done" out) (terpri out))
`).Eval(scope)
	_ = slip.ReadString(`
(defflavor blueberry () (berry))
(defmethod (blueberry :before :rot) () (princ "blueberry before rot" out) (terpri out))
(defmethod (blueberry :after :rot) () (princ "blueberry after rot" out) (terpri out))
(defwhopper (blueberry :rot) ()
 (princ "blueberry whopper rot start" out) (terpri out)
 (continue-whopper)
 (princ "blueberry whopper rot done" out) (terpri out)
)
`).Eval(scope)

	_ = slip.ReadString("(setq blue (make-instance blueberry))").Eval(scope)

	_ = slip.ReadString("(send blue :rot)").Eval(scope)
	tt.Equal(t, `blueberry whopper rot start
berry whopper rot start
blueberry before rot
berry rot
blueberry after rot
berry after rot
berry whopper rot done
blueberry whopper rot done
`, b.String())
}
