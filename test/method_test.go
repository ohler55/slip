// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
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

func TestCheckMethodArgCount(t *testing.T) {
	scope := slip.NewScope()
	inst := slip.ReadString(`(make-instance 'vanilla-flavor)`, scope).Eval(scope, nil).(slip.Instance)

	tt.Panic(t, func() { slip.CheckMethodArgCount(inst, "method-1", 1, 2, 3) })
	tt.Panic(t, func() { slip.CheckMethodArgCount(inst, "method-1", 4, 2, 3) })
}

func TestPanicMethodArgCount(t *testing.T) {
	scope := slip.NewScope()
	inst := slip.ReadString(`(make-instance 'vanilla-flavor)`, scope).Eval(scope, nil).(slip.Instance)

	tt.Panic(t, func() { slip.PanicMethodArgCount(inst, "method-1", 1, 2, 3) })
	tt.Panic(t, func() { slip.PanicMethodArgCount(inst, "method-1", 4, 2, 3) })
}

func TestPanicMethodArgChoice(t *testing.T) {
	scope := slip.NewScope()
	inst := slip.ReadString(`(make-instance 'vanilla-flavor)`, scope).Eval(scope, nil).(slip.Instance)

	tt.Panic(t, func() { slip.PanicMethodArgChoice(inst, "method-1", 1, "2 or 3") })
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
