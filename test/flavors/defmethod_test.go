// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func defineBerry(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor berry (color) ()
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables)
(defmethod (berry :rot) () "When berries rot they turn brown." (setq color 'brown))
(defmethod (berry :before :color) () (princ "berry before color") (terpri))
(defmethod (berry :after :rot) () (princ "berry after rot") (terpri))
`, scope).Eval(scope, nil)
}

func defineBlueberry(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`
(defflavor blueberry () (berry))
(defmethod (blueberry :before :rot) () "When blueberries rot they turn mushy." (princ "blueberry before rot") (terpri))
(defmethod (blueberry :after :rot) () (princ "blueberry after rot") (terpri))
`, scope).Eval(scope, nil)
}

func TestDefmethodBasic(t *testing.T) {
	defer undefFlavor("berry")
	defineBerry(t)

	scope := slip.NewScope()
	f := slip.ReadString("berry", scope).Eval(scope, nil)
	sf := f.Simplify()
	tt.Equal(t, true, jp.MustParseString("methods[?(@.name == ':rot')].combinations[*].primary").First(sf))
	tt.Equal(t, true, jp.MustParseString("methods[?(@.name == ':rot')].combinations[*].after").First(sf))
	tt.Equal(t, true, jp.MustParseString("methods[?(@.name == ':color')].combinations[*].before").First(sf))
}

func TestDefmethodInsert(t *testing.T) {
	defer undefFlavors("f1", "f2", "f3")
	scope := slip.NewScope()
	slip.ReadString(`
(defflavor f1 () ())
(defflavor f2 () (f1))
(defflavor f3 () (f2))
(defmethod (f2 :quux) () 'q2)
(defmethod (f3 :before :quux) () 'q3)
(defmethod (f1 :before :quux) () 'q1)
`, scope).Eval(scope, nil)

	f := slip.ReadString("f3", scope).Eval(scope, nil).(*flavors.Flavor)
	m := f.GetMethod(":quux")
	tt.Equal(t, `{
  combinations: [{before: true from: f3} {from: f2 primary: true} {before: true from: f1}]
  name: ":quux"
}`, pretty.SEN(m.Simplify()))
}

func TestDefmethodInherit(t *testing.T) {
	defer undefFlavors("berry", "blueberry")
	defineBerry(t)
	defineBlueberry(t)

	scope := slip.NewScope()
	f := slip.ReadString("blueberry", scope).Eval(scope, nil)
	sf := f.Simplify()
	rot := jp.MustParseString("methods[?(@.name == ':rot')].combinations[*]").Get(sf)
	tt.Equal(t, 2, len(rot))
	tt.Equal(t, "blueberry", jp.C("from").First(rot[0]))
	tt.Equal(t, true, jp.C("before").First(rot[0]))
	tt.Equal(t, true, jp.C("after").First(rot[0]))
	tt.Equal(t, "berry", jp.C("from").First(rot[1]))

	var out strings.Builder
	orig := scope.Get(slip.Symbol("*standard-output*"))
	scope.Set(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	defer scope.Set(slip.Symbol("*standard-output*"), orig)
	bb := slip.ReadString("(setq obj (make-instance 'blueberry))", scope).Eval(scope, nil).(*flavors.Instance)
	result := slip.ReadString("(send obj :rot)", scope).Eval(scope, nil)
	tt.Equal(t, "brown", slip.ObjectString(result))

	result = bb.BoundReceive(nil, ":rot", nil, 0)
	tt.Equal(t, "brown", slip.ObjectString(result))

	(&sliptest.Function{
		Scope:  scope,
		Source: `(pretty-print blueberry:before:rot nil)`,
		Expect: `"(defmethod (blueberry :before :rot) ()
  "When blueberries rot they turn mushy."
  (princ "blueberry before rot")
  (terpri))
"`,
	}).Test(t)

	// The primary is on berry and not blueberry so nil should be returned.
	(&sliptest.Function{
		Scope:  scope,
		Source: `(pretty-print blueberry:primary:rot nil)`,
		Expect: `"nil
"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(pretty-print berry:primary:rot nil)`,
		Expect: `"(defmethod (berry :primary :rot) ()
  "When berries rot they turn brown."
  (setq color 'brown))
"`,
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(pretty-print blueberry:after:rot nil)`,
		Expect: `"(defmethod (blueberry :after :rot) ()
  (princ "blueberry after rot")
  (terpri))
"`,
	}).Test(t)
}

func TestDefmethodArgCount(t *testing.T) {
	defer undefFlavor("berry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry :rot))
`, scope).Eval(scope, nil)
	})
}

func TestDefmethodNotList(t *testing.T) {
	defer undefFlavor("berry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod t ())
`, scope).Eval(scope, nil)
	})
}

func TestDefmethodLowDesignatorCount(t *testing.T) {
	defer undefFlavor("berry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry) ())
`, scope).Eval(scope, nil)
	})
}

func TestDefmethodHighDesignatorCount(t *testing.T) {
	defer undefFlavor("berry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry :before :rot nil) ())
`, scope).Eval(scope, nil)
	})
}

func TestDefmethodBadDesignator(t *testing.T) {
	defer undefFlavor("berry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry rot) ())
`, scope).Eval(scope, nil)
	})
}

func TestDefmethodBadDaemon(t *testing.T) {
	defer undefFlavor("berry")
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`
(defflavor berry (color) ())
(defmethod (berry :nothing :rot) ())
`, scope).Eval(scope, nil)
	})
}

func TestDefmethodNotFlavor(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`(defmethod (nothing :rot) ())`, scope).Eval(scope, nil)
	})
}

type rottenCaller struct{}

func (caller rottenCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	return nil
}

func (caller rottenCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":rotten",
		Text: "Something is rotten",
		Args: []*slip.DocArg{
			{Name: "bad", Type: "fixnum"},
		},
	}
}

func TestDefmethodArgsMismatch(t *testing.T) {
	defer undefFlavor("berry")
	defineBerry(t)

	scope := slip.NewScope()
	f := slip.ReadString("berry", scope).Eval(scope, nil).(*flavors.Flavor)
	tt.Panic(t, func() { f.DefMethod(":rot", ":after", rottenCaller{}) })
}
