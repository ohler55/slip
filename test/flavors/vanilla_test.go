// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors_test

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
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
	require.IsType(t, slip.Fixnum(0), id)

	has := slip.ReadString("(send berry :operation-handler-p :size)").Eval(scope)
	require.Equal(t, slip.True, has)
	has = slip.ReadString("(send berry :operation-handler-p :x)").Eval(scope)
	require.Nil(t, has)
	require.Panics(t, func() { _ = slip.ReadString("(send berry :operation-handler-p)").Eval(scope) })

	methods := slip.ReadString("(send berry :which-operations)").Eval(scope)
	require.Equal(t,
		"(:describe :id :init :operation-handler-p :print-self :send-if-handles :set-size :size :which-operations)",
		methods.String())

	pr, pw, err := os.Pipe()
	require.NoError(t, err)
	orig := slip.StandardOutput
	slip.StandardOutput = (*slip.FileStream)(pw)
	defer func() { _ = pw.Close(); _ = pr.Close(); slip.StandardOutput = orig }()

	_ = slip.ReadString("(send berry :describe)").Eval(scope)

	pw.Close()
	var out []byte
	out, err = ioutil.ReadAll(pr)
	require.NoError(t, err)
	require.Regexp(t, `#<strawberry [0-9a-f]+>, an object of flavor strawberry,
  has instance variable values:
    size: "medium"
`, string(out))

	// fmt.Printf("*** methods %s\n", methods)
}
