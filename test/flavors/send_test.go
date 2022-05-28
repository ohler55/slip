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
}

func TestSendDefHand(t *testing.T) {
	code := slip.ReadString(`
(defun seven (&rest args) args)
(defflavor handy () ()
 (:default-handler seven))
(setq hand (make-instance 'handy))
`)
	scope := slip.NewScope()
	code.Compile()
	_ = code.Eval(scope)
	defer slip.ReadString("(undefflavor 'handy)").Eval(scope)

	result := slip.ReadString("(send hand :nothing 7)").Eval(scope)
	require.Equal(t, slip.List{slip.Fixnum(7), slip.Symbol(":nothing")}, result)
}
