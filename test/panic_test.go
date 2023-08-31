// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestPanicBytes(t *testing.T) {
	f := slip.NewFunc("car", slip.List{slip.NewFunc("cdr", slip.List{slip.True})})
	msg, stack := recoverPanic(f)
	tt.Equal(t, "argument to cdr must be a cons or list not t, a t.", msg)
	tt.Equal(t, `## argument to cdr must be a cons or list not t, a t.
##  (cdr t)
##  (car (cdr t))
##  (recover)
`, stack)
}

func TestPanicArgCountLow(t *testing.T) {
	f := slip.NewFunc("car", slip.List{})
	msg, _ := recoverPanic(f)
	tt.Equal(t, "Too few arguments to car. 1 expected but got 0.", msg)

	f = slip.NewFunc("random", slip.List{})
	msg, _ = recoverPanic(f)
	tt.Equal(t, "Too few arguments to random. At least 1 expected but got 0.", msg)
}

func TestPanicArgCountHigh(t *testing.T) {
	f := slip.NewFunc("car", slip.List{nil, nil})
	msg, _ := recoverPanic(f)
	tt.Equal(t, "Too many arguments to car. 1 expected but got 2.", msg)

	f = slip.NewFunc("random", slip.List{nil, nil, slip.Fixnum(7)})
	msg, _ = recoverPanic(f)
	tt.Equal(t, "Too many arguments to random. At most 2 expected but got 3.", msg)
}

func TestArgCountCheck(t *testing.T) {
	fun := slip.NewFunc("car", slip.List{})
	tt.Panic(t, func() { slip.ArgCountCheck(fun, slip.List{}, 1, 2) })
}

func recoverPanic(obj slip.Object) (msg, stack string) {
	defer func() {
		if se, ok := recover().(slip.Error); ok {
			se.AppendToStack("recover", nil)
			msg = se.Error()
			stack = string(se.AppendFull(nil))
		}
	}()
	_ = obj.Eval(slip.NewScope(), 0)
	return
}

func TestPanicPartial(t *testing.T) {
	p := slip.NewPartial(3, "test")
	tt.Equal(t, "/^#<PARSE-ERROR [0-9a-f]+>$/", p.String())
	tt.Equal(t, "test", p.Error())
}
