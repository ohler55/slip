// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/jp"
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

	f := slip.ReadString("blueberry").Eval(scope)
	sf := f.Simplify()
	tt.Equal(t, true, jp.MustParseString("methods[*][?(@.name == ':rot')].whopper").First(sf))
}

func TestDefwhopperOneWrap(t *testing.T) {
	defer undefFlavors("berry", "blueberry")
	var b strings.Builder
	scope := slip.NewScope()
	scope.Set(slip.Symbol("out"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry :rot) () (princ "berry rot" out) (terpri out))
(defmethod (berry :after :rot) () (princ "berry after rot" out) (terpri out))
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
blueberry before rot
berry rot
blueberry after rot
berry after rot
blueberry whopper rot done
`, b.String())
}

func TestDefwhopperPanics(t *testing.T) {
	defer undefFlavors("berry")
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor berry (color) ())
`).Eval(scope)
	tt.Panic(t, func() { _ = slip.ReadString("(defwhopper ())").Eval(scope) })
	tt.Panic(t, func() { _ = slip.ReadString("(defwhopper t ())").Eval(scope) })
	tt.Panic(t, func() { _ = slip.ReadString("(defwhopper (berry) ())").Eval(scope) })
	tt.Panic(t, func() { _ = slip.ReadString("(defwhopper (berry :before :rot) ())").Eval(scope) })
	tt.Panic(t, func() { _ = slip.ReadString("(defwhopper (not-defined :rot) ())").Eval(scope) })
	tt.Panic(t, func() { _ = slip.ReadString("(defwhopper (berry t) ())").Eval(scope) })
}

func TestContinueWhopperLocation(t *testing.T) {
	defer undefFlavors("berry")
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor berry (color) ())
(defwhopper (berry :rot) () (setq loc ~whopper-location~))
`).Eval(scope)
	_ = slip.ReadString("(setq bb (make-instance berry))").Eval(scope)
	_ = slip.ReadString("(send bb :rot)").Eval(scope)
	loc := scope.Get(slip.Symbol("loc"))
	tt.Equal(t, "#<whopper-location>", slip.ObjectString(loc))
	tt.Equal(t, "#<whopper-location>", slip.Simplify(loc))
	tt.Equal(t, false, loc.Equal(nil))
	tt.Equal(t, 2, len(loc.Hierarchy()))
	tt.Equal(t, loc, loc.Eval(nil, 0))
}
