// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestFuncDocLoadForm(t *testing.T) {
	fd := slip.FuncDoc{
		Name: "quux",
		Args: []*slip.DocArg{
			{Name: "a"},
			{Name: "b", Default: slip.Fixnum(3)},
		},
	}
	tt.Equal(t, "(a (b 3))", slip.ObjectString(fd.LoadForm()))
}
