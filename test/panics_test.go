// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestPanics(t *testing.T) {
	f := slip.NewFunc("car", slip.List{slip.NewFunc("cdr", slip.List{slip.True})})
	msg := recoverPanic(f)
	tt.Equal(t, `## argument to cdr must be a cons or list not t, a t.
   (cdr t)
   (car (cdr t))
`, msg)
}

func recoverPanic(obj slip.Object) (str string) {
	defer func() {
		if p, ok := recover().(*slip.Panic); ok {
			str = string(p.Bytes())
		}
	}()
	_ = obj.Eval(slip.NewScope(), 0)
	return
}
