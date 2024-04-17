// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestRoomMinimum(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(room nil)").Eval(scope, nil)
	str := out.String()
	tt.Equal(t, "/Allocated heap/", str)
	tt.Equal(t, false, strings.Contains(str, "Heap reserved"))
	tt.Equal(t, false, strings.Contains(str, "System reserved"))
}

func TestRoomDefault(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(room)").Eval(scope, nil)
	str := out.String()
	tt.Equal(t, "/Allocated heap/", str)
	tt.Equal(t, "/Heap reserved/", str)
	tt.Equal(t, false, strings.Contains(str, "System reserved"))
}

func TestRoomTrue(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("*standard-output*"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(room t)").Eval(scope, nil)
	str := out.String()
	tt.Equal(t, "/Allocated heap/", str)
	tt.Equal(t, "/Heap reserved/", str)
	tt.Equal(t, "/System reserved/", str)
}
