// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/sliptest"
)

func TestMethodObject(t *testing.T) {
	// The type of caller doesn't matter for this test.
	fi := slip.MustFindFunc("1+", &slip.CLPkg)
	caller := fi.Create(slip.List{slip.Fixnum(3)}).(slip.Caller)

	m := slip.Method{
		Name: "sample",
		Doc: &slip.FuncDoc{
			Name: "sample",
			Args: []*slip.DocArg{
				{Name: "x"},
				{Name: "y", Default: slip.Fixnum(2)},
			},
			Return: "object",
		},
		Combinations: []*slip.Combination{
			{
				From:    slip.FindClass("fixnum"),
				Primary: caller,
			},
		},
	}
	(&sliptest.Object{
		Target: &m,
		String: `/#<method sample \(x \(y 2\)\) {[0-9a-f]+}>/`,
		Simple: sen.MustParse([]byte(`{
  combinations: [{from: fixnum primary: true}]
  name: sample
}`)),
		Hierarchy: "method.t",
		Equals: []*sliptest.EqTest{
			{Other: &m, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: &m,
	}).Test(t)
}

func TestMethodInnerCall(t *testing.T) {
	scope := slip.NewScope()
	var buf strings.Builder
	out := slip.OutputStream{Writer: &buf}
	m := slip.Method{
		Name: "sample",
		Doc:  &slip.FuncDoc{Name: "sample"},
		Combinations: []*slip.Combination{
			{
				From:    slip.FindClass("fixnum"),
				Before:  slip.ReadString(`(lambda (out) (princ 'before out))`, scope).Eval(scope, nil).(slip.Caller),
				Primary: slip.ReadString(`(lambda (out) (princ " primary " out))`, scope).Eval(scope, nil).(slip.Caller),
				After:   slip.ReadString(`(lambda (out) (princ 'after out))`, scope).Eval(scope, nil).(slip.Caller),
			},
		},
	}
	_ = m.Call(scope, slip.List{&out}, 0)
	tt.Equal(t, "before primary after", buf.String())
}

func TestMethodCallWrap(t *testing.T) {
	scope := slip.NewScope()
	var buf strings.Builder
	out := slip.OutputStream{Writer: &buf}
	m := slip.Method{
		Name: "sample",
		Doc:  &slip.FuncDoc{Name: "sample"},
		Combinations: []*slip.Combination{
			{
				From:    slip.FindClass("fixnum"),
				Wrap:    slip.ReadString(`(lambda (out) (princ 'whopper out))`, scope).Eval(scope, nil).(slip.Caller),
				Primary: slip.ReadString(`(lambda (out) (princ 'primary out))`, scope).Eval(scope, nil).(slip.Caller),
			},
		},
	}
	_ = m.Call(scope, slip.List{&out}, 0)
	tt.Equal(t, "whopper", buf.String())
}

func TestMethodArgCountCheck(t *testing.T) {
	scope := slip.NewScope()
	inst := slip.ReadString(`(make-instance 'vanilla-flavor)`, scope).Eval(scope, nil).(slip.Instance)

	tt.Panic(t, func() { slip.MethodArgCountCheck(scope, 0, inst, "method-1", 1, 2, 3) })
	tt.Panic(t, func() { slip.MethodArgCountCheck(scope, 0, inst, "method-1", 4, 2, 3) })
}

func TestMethodArgCountPanic(t *testing.T) {
	scope := slip.NewScope()
	inst := slip.ReadString(`(make-instance 'vanilla-flavor)`, scope).Eval(scope, nil).(slip.Instance)

	tt.Panic(t, func() { slip.MethodArgCountPanic(scope, 0, inst, "method-1", 1, 2, 3) })
	tt.Panic(t, func() { slip.MethodArgCountPanic(scope, 0, inst, "method-1", 4, 2, 3) })
}

func TestMethodArgChoicePanic(t *testing.T) {
	scope := slip.NewScope()
	inst := slip.ReadString(`(make-instance 'vanilla-flavor)`, scope).Eval(scope, nil).(slip.Instance)

	tt.Panic(t, func() { slip.MethodArgChoicePanic(scope, 0, inst, "method-1", 1, "2 or 3") })
}

func TestCheckSendArgCount(t *testing.T) {
	scope := slip.NewScope()
	inst := slip.ReadString(`(make-instance 'vanilla-flavor)`, scope).Eval(scope, nil).(slip.Instance)

	tt.Panic(t, func() { slip.CheckSendArgCount(scope, 0, inst, "method-1", slip.List{nil}, 2, 3) })
}

func TestMethodCompareArgs(t *testing.T) {
	// The type of caller doesn't matter for this test.
	fi := slip.MustFindFunc("1+", &slip.CLPkg)
	caller := fi.Create(slip.List{slip.Fixnum(3)}).(slip.Caller)

	m := slip.Method{
		Name: "sample",
		Doc: &slip.FuncDoc{
			Name: "sample",
			Args: []*slip.DocArg{
				{Name: "x", Type: "fixnum"},
				{Name: "y", Type: "fixnum", Default: slip.Fixnum(2)},
			},
			Return: "fixnum",
		},
		Combinations: []*slip.Combination{
			{
				From:    slip.FindClass("fixnum"),
				Primary: caller,
			},
		},
	}
	m2 := slip.Method{
		Name: "sample",
		Doc: &slip.FuncDoc{
			Name: "sample",
			Args: []*slip.DocArg{
				{Name: "a", Type: "fixnum"},
				{Name: "b", Type: "fixnum"},
			},
			Return: "fixnum",
		},
		Combinations: []*slip.Combination{
			{
				From:  slip.FindClass("fixnum"),
				After: caller,
			},
		},
	}
	// Should not panic.
	m.CompareArgs(m2.Doc)

	m2.Doc.Args[0].Type = "real"
	tt.Panic(t, func() { m.CompareArgs(m2.Doc) })

	m2.Doc.Args[0].Type = "fixnum"
	m2.Doc.Return = "object"
	tt.Panic(t, func() { m.CompareArgs(m2.Doc) })

	m2.Doc.Return = "fixnum"
	m2.Doc.Args = m2.Doc.Args[:1]
	tt.Panic(t, func() { m.CompareArgs(m2.Doc) })
}

func TestMethodBoundCall(t *testing.T) {
	defer undefFlavor("blueberry")
	defer undefFlavor("berry")
	scope := slip.NewScope()
	var buf strings.Builder
	out := slip.OutputStream{Writer: &buf}
	scope.Let(slip.Symbol("out"), &out)
	code := slip.ReadString(`
(defflavor berry () ())
;;(defwhopper (berry :double) (x) (continue-whopper (1+ x)))
(defflavor blueberry ((size "medium")) (berry) :gettable-instance-variables :settable-instance-variables)
(defmethod (blueberry :before :size) (x) (format out "before~%"))
(defmethod (blueberry :after :size) (x) (format out "after~%"))
(defmethod (blueberry :length) () 5)
(defmethod (blueberry :double) (x) (* 2 x))
(defwhopper (blueberry :double) (x) (continue-whopper (* 2 x)))
(defwhopper (berry :double) (x) (continue-whopper (1+ x)))
(setq berry (make-instance 'blueberry))
`, scope)
	berry := code.Eval(scope, nil).(*flavors.Instance)

	result := berry.BoundReceive(scope, ":length", nil, 0)
	tt.Equal(t, "5", slip.ObjectString(result))

	bindings := slip.NewScope()
	bindings.Let(slip.Symbol("x"), slip.Fixnum(3))
	result = berry.BoundReceive(scope, ":double", bindings, 0)
	tt.Equal(t, "14", slip.ObjectString(result))

	result = berry.BoundReceive(scope, ":size", nil, 0)
	tt.Equal(t, `"medium"`, slip.ObjectString(result))
	tt.Equal(t, "before\nafter\n", buf.String())
}

func undefFlavor(fn string) {
	defer func() { _ = recover() }()
	scope := slip.NewScope()
	slip.ReadString(fmt.Sprintf("(undefflavor '%s)", fn), scope).Eval(scope, nil)
}
