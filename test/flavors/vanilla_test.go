// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestVanilla(t *testing.T) {
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

	id := slip.ReadString("(send berry :id)").Eval(scope)
	tt.SameType(t, slip.Fixnum(0), id)

	has := slip.ReadString("(send berry :operation-handler-p :size)").Eval(scope)
	tt.Equal(t, slip.True, has)
	has = slip.ReadString("(send berry :operation-handler-p :x)").Eval(scope)
	tt.Nil(t, has)
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :operation-handler-p)").Eval(scope) })

	methods := slip.ReadString("(send berry :which-operations)").Eval(scope)
	tt.Equal(t,
		"(:describe :eval-inside-yourself :id :init :operation-handler-p :print-self "+
			":send-if-handles :set-size :size\n           :which-operations)",
		methods.String())

	pr, pw, err := os.Pipe()
	tt.Nil(t, err)
	orig := slip.StandardOutput
	slip.StandardOutput = (*slip.FileStream)(pw)
	defer func() { _ = pw.Close(); _ = pr.Close(); slip.StandardOutput = orig }()

	_ = slip.ReadString("(send berry :describe)").Eval(scope)

	pw.Close()
	var out []byte
	out, err = ioutil.ReadAll(pr)
	tt.Nil(t, err)
	tt.Equal(t, `/#<strawberry [0-9a-f]+>, an object of flavor strawberry,
  has instance variable values:
    size: "medium"
/`, string(out))

	pr2, pw2, err2 := os.Pipe()
	tt.Nil(t, err2)
	defer func() { _ = pw2.Close(); _ = pr2.Close() }()

	scope.Let(slip.Symbol("out"), (*slip.FileStream)(pw2))
	_ = slip.ReadString("(send berry :print-self out 0 t)").Eval(scope)
	pw2.Close()
	out, err = ioutil.ReadAll(pr2)
	tt.Nil(t, err)
	tt.Equal(t, "/#<strawberry [0-9a-f]+>/", string(out))

	tt.Panic(t, func() { _ = slip.ReadString("(send berry :print-self out 0)").Eval(scope) })
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :print-self t 0 t)").Eval(scope) })
	// try to write to a closed stream
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :print-self out 0 t)").Eval(scope) })

	size := slip.ReadString("(send berry :send-if-handles :size)").Eval(scope)
	tt.Equal(t, slip.String("medium"), size)
	result := slip.ReadString("(send berry :send-if-handles :nothing)").Eval(scope)
	tt.Nil(t, result)
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :send-if-handles)").Eval(scope) })

	size = slip.ReadString("(send berry :eval-inside-yourself 'size)").Eval(scope)
	tt.Equal(t, slip.String("medium"), size)
	tt.Panic(t, func() { _ = slip.ReadString("(send berry :eval-inside-yourself)").Eval(scope) })
}
