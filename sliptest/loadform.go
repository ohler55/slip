// Copyright (c) 2025, Peter Ohler, All rights reserved.

package sliptest

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pp"
)

// LoadForm tests pretty-print, loading, and LoadForm() calls by calling
// LoadForm() on an object followed by a pp.Append() and then a code ReadOne()
// call to recreate the object. The final test is to compare the loaded object
// with the original.
func LoadForm(t *testing.T, obj slip.Object, show ...bool) {
	lf := obj.(slip.LoadFormer)
	form := lf.LoadForm()
	scope := slip.NewScope()
	fb := pp.Append(nil, scope, form)
	if 0 < len(show) && show[0] {
		fmt.Printf("--- %s => %s [%s]\n", obj, form, bytes.TrimSpace(fb))
	}
	code, _ := slip.ReadOne(fb, scope)
	o2 := code[0]
	if _, ok := o2.(slip.List); ok {
		o2 = o2.Eval(scope, 0)
	}
	tt.Equal(t, obj.Hierarchy()[0], o2.Hierarchy()[0], "types %s != %s", obj.Hierarchy()[0], o2.Hierarchy()[0])
	tt.Equal(t, true, obj.Equal(o2), "obj: %s form: %s loaded: %s", obj, form, o2)
}
