// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestSendGetSet(t *testing.T) {
	code := slip.ReadString(`
(defflavor strawberry ((size "medium")) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size))
(setq berry (make-instance 'strawberry :size "medium"))
`)
	scope := slip.NewScope()
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'strawberry)").Eval(scope, nil)

	size := slip.ReadString("(send berry :size)").Eval(scope, nil)
	tt.Equal(t, slip.String("medium"), size)

	_ = slip.ReadString(`(send berry :set-size "large")`).Eval(scope, nil)
	size = slip.ReadString("(send berry :size)").Eval(scope, nil)
	tt.Equal(t, slip.String("large"), size)

	_ = slip.ReadString(`(setf (send berry :size) "small")`).Eval(scope, nil)
	size = slip.ReadString("(send berry :size)").Eval(scope, nil)
	tt.Equal(t, slip.String("small"), size)

	tt.Panic(t, func() { _ = slip.ReadString("(send berry :bad)").Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :set-size)").Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(setf (send t :bad) 7)").Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(setf (send berry t) 7)").Eval(scope, nil) })
}

func TestSendDefHand(t *testing.T) {
	code := slip.ReadString(`
(defun anything (&rest args) args)
(defflavor handy () ()
 (:default-handler 'anything)) ;; unquoted anything also works
(setq hand (make-instance 'handy))
`)
	scope := slip.NewScope()
	code.Compile()
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'handy)").Eval(scope, nil)

	result := slip.ReadString("(send hand :nothing 7)").Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Symbol(":nothing"), slip.Fixnum(7)}, result)

	hand := slip.ReadString("hand").Eval(scope, nil).(*flavors.Instance)
	bindings := slip.NewScope()
	bindings.Let(slip.Symbol("x"), slip.Fixnum(7))
	result = hand.BoundReceive(scope, ":nothing", bindings, 0)
	tt.Equal(t, "((x . 7))", slip.ObjectString(result))
}

func TestSendDefHandLambda(t *testing.T) {
	code := slip.ReadString(`
(defflavor handy () ()
 (:default-handler (lambda (&rest args) args)))
(setq hand (make-instance 'handy))
`)
	scope := slip.NewScope()
	code.Compile()
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'handy)").Eval(scope, nil)

	result := slip.ReadString("(send hand :nothing 7)").Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Symbol(":nothing"), slip.Fixnum(7)}, result)
}

func TestSendMissingArg(t *testing.T) {
	code := slip.ReadString(`
(defflavor missy (x) () :gettable-instance-variables)
(setq miss (make-instance 'missy))
`)
	scope := slip.NewScope()
	code.Compile()
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'missy)").Eval(scope, nil)

	tt.Panic(t, func() { _ = slip.ReadString("(send miss)").Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(send miss :describe nil nil)").Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(send miss :send-if-handles)").Eval(scope, nil) })
}

func TestSendNoMethod(t *testing.T) {
	code := slip.ReadString(`
(defflavor missy (x) () :gettable-instance-variables)
(setq miss (make-instance 'missy))
`)
	scope := slip.NewScope()
	code.Compile()
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'missy)").Eval(scope, nil)

	miss := scope.Get(slip.Symbol("miss")).(*flavors.Instance)
	tt.Panic(t, func() { _ = miss.BoundReceive(scope, ":nothing", nil, 0) })
}

func TestSendNotInstance(t *testing.T) {
	tt.Panic(t, func() { _ = slip.ReadString("(send 7 :x)").Eval(slip.NewScope(), nil) })
}

func TestSendNotKeyword(t *testing.T) {
	code := slip.ReadString(`
(defflavor nokey () ())
(setq nock (make-instance 'nokey))
`)
	scope := slip.NewScope()
	code.Compile()
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'nokey)").Eval(scope, nil)

	tt.Panic(t, func() { _ = slip.ReadString("(send nock t)").Eval(scope, nil) })
}

func TestSendDaemons(t *testing.T) {
	defer undefFlavors("berry", "blueberry")
	var b strings.Builder
	scope := slip.NewScope()
	scope.Set(slip.Symbol("out"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadString(`
(defflavor berry (color) ()
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables)
(defmethod (berry :rot) () (princ "berry rot" out) (terpri out))
(defmethod (berry :after :rot) () (princ "berry after rot" out) (terpri out))
`).Eval(scope, nil)
	_ = slip.ReadString(`
(defflavor blueberry () (berry))
(defmethod (blueberry :before :rot) () (princ "blueberry before rot" out) (terpri out))
(defmethod (blueberry :after :rot) () (princ "blueberry after rot" out) (terpri out))
`).Eval(scope, nil)

	_ = slip.ReadString("(setq blue (make-instance blueberry))").Eval(scope, nil)

	_ = slip.ReadString("(send blue :rot)").Eval(scope, nil)
	tt.Equal(t, `blueberry before rot
berry rot
blueberry after rot
berry after rot
`, b.String())
}

func TestSendClass(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-condition 'error :message "quux") :message)`,
		Expect: `"quux"`,
	}).Test(t)
}
