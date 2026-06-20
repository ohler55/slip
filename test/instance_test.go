// Copyright (c) 2026, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestInstance(t *testing.T) {
	defer undefFlavor("loaded")
	scope := slip.NewScope()
	code := slip.ReadString(`
(defflavor loaded ((size 3) (none nil)) ())
(setq load (make-instance 'loaded))
`, scope)
	obj := code.Eval(scope, nil).(slip.Instance)

	form := slip.InstanceLoadForm(obj)

	tt.Equal(t, `(let ((inst (make-instance (quote loaded)))) (setf (slot-value inst (quote none)) nil)
     (setf (slot-value inst (quote size)) 3) inst)`, slip.ObjectString(form))
}

func TestInstanceCondition(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString(`(setq quux (make-condition 'error))`, scope)
	obj := code.Eval(scope, nil).(slip.Instance)

	form := slip.InstanceLoadForm(obj)

	tt.Equal(t, `(let ((inst (make-condition (quote error)))) inst)`, slip.ObjectString(form))
}
