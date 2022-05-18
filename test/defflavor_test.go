// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

func TestDefflavorBasic(t *testing.T) {
	code := slip.ReadString(`
(defflavor strawberry ((size "medium")) ()
 :gettable-instance-variables
 :settable-instance-variables
 (:initable-instance-variables size)
 (:documentation "Strawberry icecream"))
`)
	scope := slip.NewScope()
	require.Equal(t, slip.Symbol("strawberry"), code.Eval(scope))

	f := slip.ReadString("strawberry").Eval(scope)

	// fmt.Printf("*** flavor: %s\n", pretty.SEN(f))
	sf := f.Simplify()
	require.Equal(t, "strawberry", jp.C("name").First(sf))
	require.Equal(t, "Strawberry icecream", jp.C("docs").First(sf))

	daemons := jp.MustParseString("methods[?(@.name == ':size')].daemons[*]").Get(sf)
	require.Equal(t, 1, len(daemons))

	daemons = jp.MustParseString("methods[?(@.name == ':set-size')].daemons[*]").Get(sf)
	require.Equal(t, 1, len(daemons))
}
