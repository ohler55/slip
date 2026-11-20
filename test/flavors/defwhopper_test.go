// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestDefwhopperBasic(t *testing.T) {
	undefFlavors("berry", "blueberry")
	defer undefFlavors("berry", "blueberry")
	var b strings.Builder
	scope := slip.NewScope()
	scope.Set(slip.Symbol("out"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadEvalString(`
(defflavor berry (color) ())
(defmethod (berry :after :rot) () (princ "berry after rot" out) (terpri out))
(defwhopper (berry :rot) ()
 "What a whopper!"
 (princ "berry whopper rot start" out) (terpri out)
 (continue-whopper)
 (princ "berry whopper rot done" out) (terpri out))
`, scope)
	_ = slip.ReadEvalString(`
(defflavor blueberry () (berry))
(defmethod (blueberry :before :rot) () (princ "blueberry before rot" out) (terpri out))
(defmethod (blueberry :after :rot) () (princ "blueberry after rot" out) (terpri out))
(defmethod (berry :rot) () (princ "berry rot" out) (terpri out))
(defwhopper (blueberry :rot) ()
 (princ "blueberry whopper rot start" out) (terpri out)
 (continue-whopper)
 (princ "blueberry whopper rot done" out) (terpri out)
)
`, scope)

	_ = slip.ReadEvalString("(setq blue (make-instance blueberry))", scope)

	_ = slip.ReadEvalString("(send blue :rot)", scope)
	tt.Equal(t, `blueberry whopper rot start
berry whopper rot start
blueberry before rot
berry rot
berry after rot
blueberry after rot
berry whopper rot done
blueberry whopper rot done
`, b.String())

	f := slip.ReadEvalString("blueberry", scope)
	sf := f.Simplify()
	tt.Equal(t, true, jp.MustParseString("methods[?(@.name == ':rot')].combinations[*].whopper").First(sf))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(pretty-print berry:whopper:rot nil)`,
		Expect: `"(defmethod (berry :whopper :rot) ()
  "What a whopper!"
  (princ "berry whopper rot start" out)
  (terpri out)
  (continue-whopper)
  (princ "berry whopper rot done" out)
  (terpri out))
"`,
	}).Test(t)

	tt.Equal(t, `(defmethod (blueberry :before :rot) () (princ "blueberry before rot" out) (terpri out))`,
		f.(*flavors.Flavor).DefMethodList(":rot", ":before", true).String())
	tt.Equal(t, `(defmethod (blueberry :primary :rot) () (princ "berry rot" out) (terpri out))`,
		f.(*flavors.Flavor).DefMethodList(":rot", ":primary", true).String())
	tt.Equal(t, `(defmethod (blueberry :after :rot) () (princ "blueberry after rot" out) (terpri out))`,
		f.(*flavors.Flavor).DefMethodList(":rot", ":after", true).String())
}

func TestDefwhopperOneWrap(t *testing.T) {
	defer undefFlavors("berry", "blueberry")
	var b strings.Builder
	scope := slip.NewScope()
	scope.Set(slip.Symbol("out"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadEvalString(`
(defflavor berry (color) ())
(defmethod (berry :rot) () (princ "berry rot" out) (terpri out))
(defmethod (berry :after :rot) () (princ "berry after rot" out) (terpri out))
`, scope)
	_ = slip.ReadEvalString(`
(defflavor blueberry () (berry))
(defmethod (blueberry :before :rot) () (princ "blueberry before rot" out) (terpri out))
(defmethod (blueberry :after :rot) () (princ "blueberry after rot" out) (terpri out))
(defwhopper (blueberry :rot) ()
 (princ "blueberry whopper rot start" out) (terpri out)
 (continue-whopper)
 (princ "blueberry whopper rot done" out) (terpri out)
)
`, scope)

	blue := slip.ReadEvalString("(setq blue (make-instance blueberry))", scope).(*flavors.Instance)

	_ = slip.ReadEvalString("(send blue :rot)", scope)
	tt.Equal(t, `blueberry whopper rot start
blueberry before rot
berry rot
berry after rot
blueberry after rot
blueberry whopper rot done
`, b.String())

	b.Reset()
	_ = blue.BoundReceive(nil, ":rot", nil, 0)
	tt.Equal(t, `blueberry whopper rot start
blueberry before rot
berry rot
berry after rot
blueberry after rot
blueberry whopper rot done
`, b.String())
}

func TestDefwhopperPanics(t *testing.T) {
	defer undefFlavors("berry")
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`
(defflavor berry (color) ())
`, scope)
	tt.Panic(t, func() { _ = slip.ReadEvalString("(defwhopper ())", scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString("(defwhopper t ())", scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString("(defwhopper (berry) ())", scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString("(defwhopper (berry :before :rot) ())", scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString("(defwhopper (not-defined :rot) ())", scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString("(defwhopper (berry t) ())", scope) })
}

func TestContinueWhopperLocation(t *testing.T) {
	defer undefFlavors("berry")
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`
(defflavor berry (color) ())
(defwhopper (berry :rot) () (setq loc ~whopper-location~))
`, scope)
	_ = slip.ReadEvalString("(setq bb (make-instance berry))", scope).(*flavors.Instance)
	_ = slip.ReadEvalString("(send bb :rot)", scope)
	loc := scope.Get(slip.Symbol("loc"))
	tt.Equal(t, "#<whopper-location 0>", slip.ObjectString(loc))
	tt.Equal(t, "#<whopper-location 0>", slip.Simplify(loc))
	tt.Equal(t, false, loc.Equal(nil))
	tt.Equal(t, 2, len(loc.Hierarchy()))
	tt.Equal(t, loc, loc.Eval(nil, 0))
}
