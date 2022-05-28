// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
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
	_ = code.Eval(scope)
	defer slip.ReadString("(undefflavor 'strawberry)").Eval(scope)

	size := slip.ReadString("(send berry :size)").Eval(scope)
	require.Equal(t, slip.String("medium"), size)

	_ = slip.ReadString(`(send berry :set-size "large")`).Eval(scope)
	size = slip.ReadString("(send berry :size)").Eval(scope)
	require.Equal(t, slip.String("large"), size)

	require.Panics(t, func() { _ = slip.ReadString("(send berry :bad)").Eval(scope) })
	require.Panics(t, func() { _ = slip.ReadString("(send berry :set-size)").Eval(scope) })
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
	_ = code.Eval(scope)
	defer slip.ReadString("(undefflavor 'handy)").Eval(scope)

	result := slip.ReadString("(send hand :nothing 7)").Eval(scope)
	require.Equal(t, slip.List{slip.Fixnum(7), slip.Symbol(":nothing")}, result)
}

func TestSendMissingArg(t *testing.T) {
	code := slip.ReadString(`
(defflavor missy (x) () :gettable-instance-variables)
(setq miss (make-instance 'missy))
`)
	scope := slip.NewScope()
	code.Compile()
	_ = code.Eval(scope)
	defer slip.ReadString("(undefflavor 'missy)").Eval(scope)

	require.Panics(t, func() { _ = slip.ReadString("(send miss)").Eval(scope) })
}

func TestSendNotInstance(t *testing.T) {
	require.Panics(t, func() { _ = slip.ReadString("(send 7 :x)").Eval(slip.NewScope()) })
}

func TestSendNotKeyword(t *testing.T) {
	code := slip.ReadString(`
(defflavor nokey () ())
(setq nock (make-instance 'nokey))
`)
	scope := slip.NewScope()
	code.Compile()
	_ = code.Eval(scope)
	defer slip.ReadString("(undefflavor 'nokey)").Eval(scope)

	require.Panics(t, func() { _ = slip.ReadString("(send nock t)").Eval(scope) })
}
