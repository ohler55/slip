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
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor strawberry ((size "medium")) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size))
(setq berry (make-instance 'strawberry :size "medium"))
`, scope)
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'strawberry)", scope).Eval(scope, nil)

	size := slip.ReadString("(send berry :size)", scope).Eval(scope, nil)
	tt.Equal(t, slip.String("medium"), size)

	_ = slip.ReadString(`(send berry :set-size "large")`, scope).Eval(scope, nil)
	size = slip.ReadString("(send berry :size)", scope).Eval(scope, nil)
	tt.Equal(t, slip.String("large"), size)

	_ = slip.ReadString(`(setf (send berry :size) "small")`, scope).Eval(scope, nil)
	size = slip.ReadString("(send berry :size)", scope).Eval(scope, nil)
	tt.Equal(t, slip.String("small"), size)

	tt.Panic(t, func() { _ = slip.ReadString("(send berry :bad)", scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :set-size)", scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(setf (send t :bad) 7)", scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(setf (send berry t) 7)", scope).Eval(scope, nil) })
}

func TestSendDefHand(t *testing.T) {
	scope := slip.NewScope()
	defer slip.ReadString("(fmakunbound 'anything)", scope).Eval(scope, nil)

	code := slip.ReadString(`
(defun anything (&rest args) args)
(defflavor handy () ()
 (:default-handler 'anything)) ;; unquoted anything also works
(setq hand (make-instance 'handy))
`, scope)
	code.Compile()
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'handy)", scope).Eval(scope, nil)

	result := slip.ReadString("(send hand :nothing 7)", scope).Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Symbol(":nothing"), slip.Fixnum(7)}, result)

	hand := slip.ReadString("hand", scope).Eval(scope, nil).(*flavors.Instance)
	bindings := slip.NewScope()
	bindings.Let(slip.Symbol("x"), slip.Fixnum(7))
	result = hand.BoundReceive(scope, ":nothing", bindings, 0)
	tt.Equal(t, "((x . 7))", slip.ObjectString(result))
}

func TestSendDefHandLambda(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor handy () ()
 (:default-handler (lambda (&rest args) args)))
(setq hand (make-instance 'handy))
`, scope)
	code.Compile()
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'handy)", scope).Eval(scope, nil)

	result := slip.ReadString("(send hand :nothing 7)", scope).Eval(scope, nil)
	tt.Equal(t, slip.List{slip.Symbol(":nothing"), slip.Fixnum(7)}, result)
}

func TestSendMissingArg(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor missy (x) () :gettable-instance-variables)
(setq miss (make-instance 'missy))
`, scope)
	code.Compile()
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'missy)", scope).Eval(scope, nil)

	tt.Panic(t, func() { _ = slip.ReadString("(send miss)", scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(send miss :describe nil nil)", scope).Eval(scope, nil) })
	tt.Panic(t, func() { _ = slip.ReadString("(send miss :send-if-handles)", scope).Eval(scope, nil) })
}

func TestSendNoMethod(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor missy (x) () :gettable-instance-variables)
(setq miss (make-instance 'missy))
`, scope)
	code.Compile()
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'missy)", scope).Eval(scope, nil)

	miss := scope.Get(slip.Symbol("miss")).(*flavors.Instance)
	tt.Panic(t, func() { _ = miss.BoundReceive(scope, ":nothing", nil, 0) })
}

func TestSendNotInstance(t *testing.T) {
	(&sliptest.Function{
		Source:    `(send t :x)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSendNotKeyword(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor nokey () ())
(setq nock (make-instance 'nokey))
`, scope)
	code.Compile()
	_ = code.Eval(scope, nil)
	defer slip.ReadString("(undefflavor 'nokey)", scope).Eval(scope, nil)

	tt.Panic(t, func() { _ = slip.ReadString("(send nock t)", scope).Eval(scope, nil) })
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
`, scope).Eval(scope, nil)
	_ = slip.ReadString(`
(defflavor blueberry () (berry))
(defmethod (blueberry :before :rot) () (princ "blueberry before rot" out) (terpri out))
(defmethod (blueberry :after :rot) () (princ "blueberry after rot" out) (terpri out))
`, scope).Eval(scope, nil)

	_ = slip.ReadString("(setq blue (make-instance blueberry))", scope).Eval(scope, nil)

	_ = slip.ReadString("(send blue :rot)", scope).Eval(scope, nil)
	tt.Equal(t, `blueberry before rot
berry rot
berry after rot
blueberry after rot
`, b.String())
}

func TestSendNil(t *testing.T) {
	(&sliptest.Function{
		Source:    `(send nil :message)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSendClassMethodNotKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    `(send (make-condition 'error :message "quux") 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
