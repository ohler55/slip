// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestPanicBytes(t *testing.T) {
	f := slip.NewFunc("car", slip.List{slip.NewFunc("cdr", slip.List{slip.True})})
	p, msg, stack := recoverPanic(f)
	tt.Equal(t, "arg must be a cons or list not t, a t.", msg)
	tt.Equal(t, `## arg must be a cons or list not t, a t.
##  (cdr t)
##  (car (cdr t))
##  (recover)
`, stack)
	tt.Equal(t, []string{"(cdr t)", "(car (cdr t))", "(recover)"}, p.Stack())
	tt.Equal(t, p, p.Eval(nil, 0))
	px := p
	tt.Equal(t, true, p.Equal(px))
	p.Condition = nil
	tt.Equal(t, `## arg must be a cons or list not t, a t.
##  (cdr t)
##  (car (cdr t))
##  (recover)
`, string(p.AppendFull(nil)))
}

func TestPanicArgCountLow(t *testing.T) {
	f := slip.NewFunc("car", slip.List{})
	_, msg, _ := recoverPanic(f)
	tt.Equal(t, "Too few arguments to car. 1 expected but got 0.", msg)

	f = slip.NewFunc("random", slip.List{})
	_, msg, _ = recoverPanic(f)
	tt.Equal(t, "Too few arguments to random. At least 1 expected but got 0.", msg)
}

func TestPanicArgCountHigh(t *testing.T) {
	f := slip.NewFunc("car", slip.List{nil, nil})
	_, msg, _ := recoverPanic(f)
	tt.Equal(t, "Too many arguments to car. 1 expected but got 2.", msg)

	f = slip.NewFunc("random", slip.List{nil, nil, slip.Fixnum(7)})
	_, msg, _ = recoverPanic(f)
	tt.Equal(t, "Too many arguments to random. At most 2 expected but got 3.", msg)
}

func TestPanicAppend(t *testing.T) {
	p := slip.Panic{
		Message:   "pan ick",
		Value:     slip.Fixnum(3),
		Condition: slip.NewWarning("warn").(slip.Instance),
	}
	tt.Equal(t, "/#<warning [0-9a-f]+>/", p.String())
	tt.Equal(t, "/#<warning [0-9a-f]+>/", p.Simplify())
	p.Message = ""
	tt.Equal(t, "/#<warning [0-9a-f]+>/", p.Error())
}

func TestArgCountCheck(t *testing.T) {
	fun := slip.NewFunc("car", slip.List{})
	tt.Panic(t, func() { slip.ArgCountCheck(fun, slip.List{}, 1, 2) })
}

func recoverPanic(obj slip.Object) (se *slip.Panic, msg, stack string) {
	defer func() {
		if se, _ = recover().(*slip.Panic); se != nil {
			se.AppendToStack("recover", nil)
			msg = se.Error()
			stack = string(se.AppendFull(nil))
		}
	}()
	_ = obj.Eval(slip.NewScope(), 0)
	return
}
