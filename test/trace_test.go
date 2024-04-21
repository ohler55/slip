// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"bytes"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestTraceAll(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*trace-output*"), &slip.OutputStream{Writer: &out})

	defer slip.Untrace(nil)
	slip.Trace(slip.List{slip.True})
	_ = slip.ReadString("(car (car (car '(((a))))))").Eval(scope, nil)

	tt.Equal(t, `0: (car (car (car '(((a))))))
  1: (car (car '(((a)))))
    2: (car '(((a))))
      3: (quote (((a))))
      3: (quote (((a)))) => (((a)))
    2: (car '(((a)))) => ((a))
  1: (car (car '(((a))))) => (a)
0: (car (car (car '(((a)))))) => a
`, out.String())

	tt.Equal(t, "(t)", slip.ObjectString(slip.ReadString("(trace)").Eval(scope, nil)))
}

func TestTraceNames(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*trace-output*"), &slip.OutputStream{Writer: &out})

	defer slip.Untrace(nil)
	slip.Trace(slip.List{slip.Symbol("car"), slip.Symbol("cdr")})

	_ = slip.ReadString("(car (car (car '(((a))))))").Eval(scope, nil)
	tt.Equal(t, `0: (car (car (car '(((a))))))
  1: (car (car '(((a)))))
    2: (car '(((a))))
    2: (car '(((a)))) => ((a))
  1: (car (car '(((a))))) => (a)
0: (car (car (car '(((a)))))) => a
`, out.String())

	tt.Equal(t, "(car cdr)", slip.ObjectString(slip.ReadString("(trace)").Eval(scope, nil)))
	_ = slip.ObjectString(slip.ReadString("(untrace car)").Eval(scope, nil))
	tt.Equal(t, "(cdr)", slip.ObjectString(slip.ReadString("(trace)").Eval(scope, nil)))
}

func TestTracePanicANSI(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*trace-output*"), &slip.OutputStream{Writer: &out})

	slip.CurrentPackage.Set("*repl-warning-ansi*", slip.String("\x1b[31m"))

	slip.Trace(slip.List{slip.True})
	tt.Panic(t, func() { _ = slip.ReadString("(car (car (car 7)))").Eval(scope, nil) })
	slip.Untrace(nil)

	tt.Equal(t, "0: (car (car (car 7)))\n"+
		"  1: (car (car 7))\n"+
		"    2: (car 7)\n"+
		"    2: (car 7) => \x1b[31margument to car must be a cons or list not 7, a fixnum.\x1b[m\n"+
		"  1: (car (car 7)) => \x1b[31margument to car must be a cons or list not 7, a fixnum.\x1b[m\n"+
		"0: (car (car (car 7))) => \x1b[31margument to car must be a cons or list not 7, a fixnum.\x1b[m\n",
		out.String())
}

func TestTracePanicNotANSI(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*trace-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	slip.Trace(slip.List{slip.True})
	tt.Panic(t, func() { _ = slip.ReadString("(car (car (car 7)))").Eval(scope, nil) })
	slip.Untrace(nil)

	tt.Equal(t, "0: (car (car (car 7)))\n"+
		"  1: (car (car 7))\n"+
		"    2: (car 7)\n"+
		"    2: (car 7) => argument to car must be a cons or list not 7, a fixnum.\n"+
		"  1: (car (car 7)) => argument to car must be a cons or list not 7, a fixnum.\n"+
		"0: (car (car (car 7))) => argument to car must be a cons or list not 7, a fixnum.\n",
		out.String())
}

func TestTracePanicDeep(t *testing.T) {
	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*trace-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	slip.Trace(slip.List{slip.True})
	var deep []byte
	cnt := 42
	for i := cnt; 0 <= i; i-- {
		deep = append(deep, "(car "...)
	}
	for i := cnt; 0 <= i; i-- {
		deep = append(deep, ')')
	}
	tt.Panic(t, func() { _ = slip.Read(deep).Eval(scope, nil) })
	slip.Untrace(nil)
	// Indent is capped at 80 spaces.
	tt.Equal(t, false, strings.Contains(out.String(), strings.Repeat(" ", 81)))
}

func TestTracePanicString(t *testing.T) {
	slip.Define(
		func(args slip.List) slip.Object {
			f := panicTest{Function: slip.Function{Name: "panic-test", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "panic-test",
			Args:   []*slip.DocArg{},
			Return: "nil",
		}, &slip.UserPkg)

	var out bytes.Buffer
	scope := slip.NewScope()
	scope.Let(slip.Symbol("*trace-output*"), &slip.OutputStream{Writer: &out})
	scope.Let("*print-ansi*", nil)
	slip.Trace(slip.List{slip.True})
	tt.Panic(t, func() { _ = slip.ReadString("(panic-test)").Eval(scope, nil) })
	slip.Untrace(nil)

	tt.Equal(t, `0: (panic-test)
0: (panic-test) => string panic
`,
		out.String())
}

type panicTest struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *panicTest) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	panic("string panic")
}
