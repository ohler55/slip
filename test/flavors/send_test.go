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
	_ = slip.ReadEvalString(`
(defflavor strawberry ((size "medium")) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size))
(setq berry (make-instance 'strawberry :size "medium"))
`, scope)

	defer func() { _ = slip.ReadEvalString("(undefflavor 'strawberry)", scope) }()

	size := slip.ReadEvalString("(send berry :size)", scope)
	tt.Equal(t, slip.String("medium"), size)

	_ = slip.ReadEvalString(`(send berry :set-size "large")`, scope)
	size = slip.ReadEvalString("(send berry :size)", scope)
	tt.Equal(t, slip.String("large"), size)

	_ = slip.ReadEvalString(`(setf (send berry :size) "small")`, scope)
	size = slip.ReadEvalString("(send berry :size)", scope)
	tt.Equal(t, slip.String("small"), size)

	tt.Panic(t, func() { _ = slip.ReadEvalString("(send berry :bad)", scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString("(send berry :set-size)", scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString("(setf (send t :bad) 7)", scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString("(setf (send berry t) 7)", scope) })
}

func TestSendDefHand(t *testing.T) {
	scope := slip.NewScope()
	defer func() { _ = slip.ReadEvalString("(fmakunbound 'anything)", scope) }()

	_ = slip.ReadEvalString(`
(defun anything (&rest args) args)
(defflavor handy () ()
 (:default-handler 'anything)) ;; unquoted anything also works
(setq hand (make-instance 'handy))
`, scope)
	defer func() { _ = slip.ReadEvalString("(undefflavor 'handy)", scope) }()

	result := slip.ReadEvalString("(send hand :nothing 7)", scope)
	tt.Equal(t, slip.List{slip.Symbol(":nothing"), slip.Fixnum(7)}, result)

	hand := slip.ReadEvalString("hand", scope).(*flavors.Instance)
	bindings := slip.NewScope()
	bindings.Let(slip.Symbol("x"), slip.Fixnum(7))
	result = hand.BoundReceive(scope, ":nothing", bindings, 0)
	tt.Equal(t, "((x . 7))", slip.ObjectString(result))
}

func TestSendDefHandLambda(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`
(defflavor handy () ()
 (:default-handler (lambda (&rest args) args)))
(setq hand (make-instance 'handy))
`, scope)
	defer func() { _ = slip.ReadEvalString("(undefflavor 'handy)", scope) }()

	result := slip.ReadEvalString("(send hand :nothing 7)", scope)
	tt.Equal(t, slip.List{slip.Symbol(":nothing"), slip.Fixnum(7)}, result)
}

func TestSendMissingArg(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`
(defflavor missy (x) () :gettable-instance-variables)
(setq miss (make-instance 'missy))
`, scope)
	defer slip.ReadEvalString("(undefflavor 'missy)", scope)

	tt.Panic(t, func() { _ = slip.ReadEvalString("(send miss)", scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString("(send miss :describe nil nil)", scope) })
	tt.Panic(t, func() { _ = slip.ReadEvalString("(send miss :send-if-handles)", scope) })
}

func TestSendNoMethod(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadEvalString(`
(defflavor missy (x) () :gettable-instance-variables)
(setq miss (make-instance 'missy))
`, scope)
	defer slip.ReadEvalString("(undefflavor 'missy)", scope)

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
	_ = slip.ReadEvalString(`
(defflavor nokey () ())
(setq nock (make-instance 'nokey))
`, scope)
	defer slip.ReadEvalString("(undefflavor 'nokey)", scope)

	tt.Panic(t, func() { _ = slip.ReadEvalString("(send nock t)", scope) })
}

func TestSendDaemons(t *testing.T) {
	defer undefFlavors("berry", "blueberry")
	var b strings.Builder
	scope := slip.NewScope()
	scope.Set(slip.Symbol("out"), &slip.OutputStream{Writer: &b})
	_ = slip.ReadEvalString(`
(defflavor berry (color) ()
 :gettable-instance-variables
 :settable-instance-variables
 :initable-instance-variables)
(defmethod (berry :rot) () (princ "berry rot" out) (terpri out))
(defmethod (berry :after :rot) () (princ "berry after rot" out) (terpri out))
`, scope)
	_ = slip.ReadEvalString(`
(defflavor blueberry () (berry))
(defmethod (blueberry :before :rot) () (princ "blueberry before rot" out) (terpri out))
(defmethod (blueberry :after :rot) () (princ "blueberry after rot" out) (terpri out))
`, scope)

	_ = slip.ReadEvalString("(setq blue (make-instance blueberry))", scope)

	_ = slip.ReadEvalString("(send blue :rot)", scope)
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
