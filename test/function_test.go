// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestFunctionNew(t *testing.T) {
	cl := slip.FindPackage("common-lisp")
	f := slip.NewFunc("car", slip.List{nil}, cl)
	tt.Equal(t, "(car nil)", slip.ObjectString(f))

	tt.Panic(t, func() { _ = slip.NewFunc("nothing", slip.List{}) })
}

func TestFunctionFind(t *testing.T) {
	cl := slip.FindPackage("common-lisp")
	f := slip.FindFunc("car", cl)
	tt.NotNil(t, f)

	tt.Panic(t, func() { _ = slip.FindFunc("nothing") })
}
