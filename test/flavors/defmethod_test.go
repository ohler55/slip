// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defineBerry(t *testing.T) {
	_ = slip.ReadString(`
(defflavor berry (color) ()
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables)
(defmethod (berry :rot) () "When berries rot they turn brown." (setq color 'brown))
(defmethod (berry :before :color) () (princ "berry before color") (terpri))
(defmethod (berry :after :rot) () (princ "berry after rot") (terpri))
`).Eval(slip.NewScope(), nil)
}

func defineBlueberry(t *testing.T) {
	_ = slip.ReadString(`
(defflavor blueberry () (berry))
(defmethod (blueberry :before :rot) () "Berries that are blue." (princ "blueberry before rot") (terpri))
(defmethod (blueberry :after :rot) () (princ "blueberry after rot") (terpri))
`).Eval(slip.NewScope(), nil)
}

func TestDefmethodBasic(t *testing.T) {
	defer undefFlavor("berry")
	defineBerry(t)

	f := slip.ReadString("berry").Eval(slip.NewScope(), nil)
	sf := f.Simplify()
	tt.Equal(t, true, jp.MustParseString("methods[*][?(@.name == ':rot')].primary").First(sf))
	tt.Equal(t, true, jp.MustParseString("methods[*][?(@.name == ':rot')].after").First(sf))
	tt.Equal(t, true, jp.MustParseString("methods[*][?(@.name == ':color')].before").First(sf))
}

func TestDefmethodInherit(t *testing.T) {
	defer undefFlavors("berry", "blueberry")
	defineBerry(t)
	defineBlueberry(t)

	f := slip.ReadString("blueberry").Eval(slip.NewScope(), nil)
	sf := f.Simplify()
	rot := jp.MustParseString("methods[*][?(@.name == ':rot')]").Get(sf)
	tt.Equal(t, 2, len(rot))
	tt.Equal(t, "blueberry", jp.C("from").First(rot[0]))
	tt.Equal(t, true, jp.C("before").First(rot[0]))
	tt.Equal(t, true, jp.C("after").First(rot[0]))
	tt.Equal(t, "berry", jp.C("from").First(rot[1]))

	var out strings.Builder
	scope := slip.NewScope()
	orig := scope.Get(slip.Symbol("*standard-output*"))
	scope.Set(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	defer scope.Set(slip.Symbol("*standard-output*"), orig)
	bb := slip.ReadString("(setq obj (make-instance 'blueberry))").Eval(scope, nil).(*flavors.Instance)
	result := slip.ReadString("(send obj :rot)").Eval(scope, nil)
	tt.Equal(t, "brown", slip.ObjectString(result))

	result = bb.BoundReceive(nil, ":rot", nil, 0)
	tt.Equal(t, "brown", slip.ObjectString(result))
}

func TestDefmethodArgCount(t *testing.T) {
	defer undefFlavor("berry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry :rot))
`).Eval(slip.NewScope(), nil)
	})
}

func TestDefmethodNotList(t *testing.T) {
	defer undefFlavor("berry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod t ())
`).Eval(slip.NewScope(), nil)
	})
}

func TestDefmethodLowDesignatorCount(t *testing.T) {
	defer undefFlavor("berry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry) ())
`).Eval(slip.NewScope(), nil)
	})
}

func TestDefmethodHighDesignatorCount(t *testing.T) {
	defer undefFlavor("berry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry :before :rot nil) ())
`).Eval(slip.NewScope(), nil)
	})
}

func TestDefmethodBadDesignator(t *testing.T) {
	defer undefFlavor("berry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry rot) ())
`).Eval(slip.NewScope(), nil)
	})
}

func TestDefmethodBadDaemon(t *testing.T) {
	defer undefFlavor("berry")
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry :nothing :rot) ())
`).Eval(slip.NewScope(), nil)
	})
}

func TestDefmethodNotFlavor(t *testing.T) {
	tt.Panic(t, func() {
		_ = slip.ReadString(`(defmethod (nothing :rot) ())`).Eval(slip.NewScope(), nil)
	})
}
