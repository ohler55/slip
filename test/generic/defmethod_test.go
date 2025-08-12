// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/generic"
	"github.com/ohler55/slip/sliptest"
)

func undefFlavors(fns ...string) {
	for _, fn := range fns {
		undefFlavor(fn)
	}
}

func undefFlavor(fn string) {
	defer func() { _ = recover() }()
	scope := slip.NewScope()
	slip.ReadString(fmt.Sprintf("(undefflavor '%s)", fn), scope).Eval(scope, nil)
}

func TestDefmethodFlavors(t *testing.T) {
	defer undefFlavor("berry")
	(&sliptest.Function{
		Source: `(progn
                   (defflavor berry (color) ()
                                    :gettable-instance-variables
                                    :settable-instance-variables
                                    :initable-instance-variables)
                   (defmethod (berry :rot) () "When berries rot they turn brown." (setq color 'brown)))`,
		Expect: `/#<method :rot nil \{[0-9a-f]+\}>/`,
	}).Test(t)
}

func TestDefmethodBadMethod(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defmethod 7 () (setq color 'brown))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefClassMethodBadClass(t *testing.T) {
	tt.Panic(t, func() { generic.DefClassMethod(slip.FindClass("fixnum"), ":quux", "", nil) })
}

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
(defwhopper (f1 :quux) nil)
`, scope).Eval(scope, nil)

	f := slip.ReadString("f3", scope).Eval(scope, nil).(*flavors.Flavor)
	m := f.GetMethod(":quux")
	tt.Equal(t, `{
  combinations: [
    {before: true from: f3}
    {from: f2 primary: true}
    {before: true from: f1 whopper: true}
  ]
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

func TestDefmethodGenericOrdinaryError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(defmethod defmethod ((a fixnum)) nil)`,
		PanicType: slip.ProgramErrorSymbol,
	}).Test(t)
}

func TestDefmethodGenericAutoDef(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defmethod quux (a (b fixnum)) (+ a b))`,
		Expect: `/#<method quux \(a \(b fixnum\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(quux 1 2)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(quux 1 2.1)`,
		PanicType: slip.NoApplicableMethodErrorSymbol,
	}).Test(t)
}

func TestDefmethodGenericCallNextMethodError(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defmethod quux (a (b fixnum)) (call-next-method))`,
		Expect: `/#<method quux \(a \(b fixnum\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(quux 1 2)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestDefmethodGenericDocs(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(documentation (defmethod quux (a (b fixnum)) "quacker" (+ a b)) 'method)`,
		Expect: `"quacker"`,
	}).Test(t)
}

func TestDefmethodGenericBadLambdaList(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source:    `(defmethod quux t nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defmethod quux (t) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defmethod quux ((7)) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefmethodGenericQualifier(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defmethod quux ((s output-stream) (x fixnum)) (format s "primary"))`,
		Expect: `/#<method quux \(\(s output-stream\) \(x fixnum\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(defmethod quux :after ((s output-stream) (x real)) (format s "-after"))`,
		Expect: `/#<method quux :after \(\(s output-stream\) \(x real\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(with-output-to-string(os) (quux os 3))`,
		Expect: `"primary-after"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(defmethod quux :before ((s output-stream) (x real)) (format s "before-"))`,
		Expect: `/#<method quux :before :after \(\(s output-stream\) \(x real\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(defmethod quux :around ((s output-stream) (x fixnum))
                   (format s "around-")
                   (call-next-method)
                   (format s "-around"))`,
		Expect: `/#<method quux :around \(\(s output-stream\) \(x fixnum\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(with-output-to-string(os) (quux os 3))`,
		Expect: `"around-before-primary-after-around"`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defmethod quux :not-a-qualifier (x) nil)`,
		PanicType: slip.InvalidMethodErrorSymbol,
	}).Test(t)
}

func TestDefmethodGenericLambdaList(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defmethod quux ((a t) (b t)) (list a b))`,
		Expect: `/#<method quux \(\(a t\) \(b t\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(quux 'x 1)`,
		Expect: "(x 1)",
	}).Test(t)
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source:    `(defmethod quux ((a 7)) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defmethod quux ((a)) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defmethod quux (7) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefmethodGenericOptionalValue(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defmethod quux ((a t) &optional (b 7)) nil)`,
		Expect: `/#<method quux \(\(a t\) &optional \(b 7\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
}

func TestDefmethodGenericBadOptional(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(defgeneric quux (a &optional b))`,
		Expect: "#<generic-function quux>",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defmethod quux ((a t) &optional 7) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(defmethod quux ((a t) &optional (x)) nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDefmethodGenericSetf(t *testing.T) {
	slip.CurrentPackage.Undefine("(setf quux)")
	(&sliptest.Function{
		Source: `(defmethod (setf quux) ((x fixnum)) (list x))`,
		Expect: `/#<method \(setf quux\) \(\(x fixnum\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
	// repeat
	(&sliptest.Function{
		Source: `(defmethod (setf quux) ((x fixnum)) (list x))`,
		Expect: `/#<method \(setf quux\) \(\(x fixnum\)\) \{[0-9a-f]+\}>/`,
	}).Test(t)
}

func TestDefmethodCaller(t *testing.T) {
	for _, name := range []string{"quux",
		"quux-x", "quux-y",
		"quux-set-x", "quux-set-y",
		"quux-acc-x", "quux-acc-y",
		"(setf-acc-x)", "(setf-acc-y)",
	} {
		slip.CurrentPackage.Remove(name)
	}
	(&sliptest.Function{
		Source: `(progn
                   (defclass quux ()
                     ((x :initform 7 :reader quux-x :writer quux-set-x :accessor quux-acc-x :gettable t :settable t)
                      (y :initform 2 :reader quux-y :writer quux-set-y :accessor quux-acc-y :allocation :class)))
                   (let* ((q (make-instance 'quux))
                          (result (list (quux-x q))))
                     (quux-set-x q 3)
                     (addf result (quux-x q))
                     (setf (quux-acc-x q) 4)
                     (addf result (quux-x q))
                     (quux-set-y q 4)
                     (addf result (quux-y q))
                     (setf (quux-acc-y q) 6)
                     (addf result (quux-y q))
                     (send q :set-x 1)
                     (addf result (send q :x))
                     result))`,
		Expect: "(7 3 4 4 6 1)",
	}).Test(t)
}

func TestDefmethodGenericLoadForm(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(progn
                   (defgeneric quux (a b)
                     (:documentation "quack quack")
                     (:method ((a fixnum) (b fixnum)) (list a b))
                     (:method :before ((a fixnum) (b fixnum)) (princ a))
                     (:method :after ((a fixnum) (b fixnum)) (princ b))
                     (:method :around ((a fixnum) (b real)) (call-next-method a b)))
                   (let ((*print-right-margin* 80))
                     (pretty-print quux nil)))`,
		Expect: `"(defgeneric quux (a b)
  (:documentation "quack quack")
  (:method ((a fixnum) (b fixnum))
    "quack quack"
    (list a b))
  (:method :before ((a fixnum) (b fixnum))
    "quack quack"
    (princ a))
  (:method :after ((a fixnum) (b fixnum))
    "quack quack"
    (princ b))
  (:method :around ((a fixnum) (b real))
    "quack quack"
    (call-next-method a b)))
"`,
	}).Test(t)
}

func TestDefmethodGenericLoadFormDoc(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(progn
                   (defgeneric quux (a b))
                   (defmethod quux ((a fixnum) (b fixnum)) "quack quack" (list a b))
                   (let ((*print-right-margin* 80))
                     (pretty-print quux nil)))`,
		Expect: `"(defgeneric quux (a b)
  (:method ((a fixnum) (b fixnum))
    "quack quack"
    (list a b)))
"`,
	}).Test(t)
}

func TestDefmethodGenericLoadFormLambdaList(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(progn
                   (defgeneric quux (a &optional b c))
                   (defmethod quux ((a fixnum) &optional (b 5) c) (list a b c))
                   (let ((*print-right-margin* 80))
                     (pretty-print quux nil)))`,
		Expect: `"(defgeneric quux (a &optional b c)
  (:method ((a fixnum) &optional (b 5) c)
    (list a b c)))
"`,
	}).Test(t)
}

func TestDefmethodGenericLoadFormEmpty(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	(&sliptest.Function{
		Source: `(progn
                   (defgeneric quux (a b) (:documentation "quack quack"))
                   (let ((*print-right-margin* 80))
                     (pretty-print quux nil)))`,
		Expect: `"(defgeneric quux (a b)
  (:documentation "quack quack"))
"`,
	}).Test(t)
}

func TestDefmethodCallerOrdinary(t *testing.T) {
	tt.Panic(t, func() { _ = generic.DefCallerMethod("", nil, &slip.FuncDoc{Name: "coerce"}) })
}

func TestDefmethodCallerOptional(t *testing.T) {
	slip.CurrentPackage.Undefine("quux")
	// Make sure it does not fail.
	_ = generic.DefCallerMethod("", nil,
		&slip.FuncDoc{
			Name: "quux",
			Args: []*slip.DocArg{{Name: "x"}, {Name: "&optional"}, {Name: "y"}},
		})
}
