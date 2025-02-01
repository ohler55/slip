// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
	"github.com/ohler55/slip/sliptest"
)

func TestClassBasic(t *testing.T) {
	c := slip.ReadString(`(find-class 'fixnum)`).Eval(slip.NewScope(), nil)
	c2 := c
	tt.Equal(t, c, c.Eval(nil, 0))
	tt.Equal(t, true, c.Equal(c2))
	tt.Equal(t, false, c.Equal(nil))
	tt.Equal(t, "[class standard-object t]", pretty.SEN(c.Hierarchy()))
	tt.Equal(t, "built-in fixed number class", c.(slip.Class).Documentation())
	tt.Equal(t, "#<class fixnum>", c.String())
	tt.Equal(t, `{
  docs: "built-in fixed number class"
  final: true
  inherit: [integer]
  methods: [
    ":describe"
    ":equal"
    ":eval-inside-yourself"
    ":flavor"
    ":id"
    ":init"
    ":inspect"
    ":operation-handled-p"
    ":print-self"
    ":send-if-handles"
    ":shared-initialize"
    ":update-instance-for-different-class"
    ":which-operations"
  ]
  name: fixnum
  prototype: 42
  slots: {}
}`, pretty.SEN(c.Simplify()))
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'fixnum)`).Eval(slip.NewScope(), nil) })
}

func TestClassDescribeBasic(t *testing.T) {
	c := slip.ReadString(`(find-class 'fixnum)`).Eval(slip.NewScope(), nil).(slip.Class)
	out := c.Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `fixnum is a built-in class:
  Documentation:
    built-in fixed number class
  Direct superclasses: integer
  Class precedence list: integer rational real number built-in-class
  Slots: None
  Methods:
    :describe
    :equal
    :eval-inside-yourself
    :flavor
    :id
    :init
    :inspect
    :operation-handled-p
    :print-self
    :send-if-handles
    :shared-initialize
    :update-instance-for-different-class
    :which-operations
  Prototype: 42
`, string(out))

	out = c.Describe([]byte{}, 0, 80, true)
	tt.Equal(t, "\x1b[1mfixnum\x1b[m is a built-in class:\n"+
		"  Documentation:\n"+
		"    built-in fixed number class\n"+
		"  Direct superclasses: integer\n"+
		"  Class precedence list: integer rational real number built-in-class\n"+
		"  Slots: None\n"+
		"  Methods:\n"+
		"    :describe\n"+
		"    :equal\n"+
		"    :eval-inside-yourself\n"+
		"    :flavor\n"+
		"    :id\n"+
		"    :init\n"+
		"    :inspect\n"+
		"    :operation-handled-p\n"+
		"    :print-self\n"+
		"    :send-if-handles\n"+
		"    :shared-initialize\n"+
		"    :update-instance-for-different-class\n"+
		"    :which-operations\n"+
		"  Prototype: 42\n", string(out))
}

func TestClassDefClass(t *testing.T) {
	c := clos.DefClass(
		"dummy",
		"dummy class",
		map[string]slip.Object{"x": slip.Fixnum(3), "y": slip.Fixnum(5)},
		nil, // supers
		false,
	)
	c.DefMethod(":fun", "", nil)
	found := slip.ReadString(`(find-class 'dummy)`).Eval(slip.NewScope(), nil).(slip.Class)
	tt.Equal(t, c, found)
	out := c.Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `dummy is a class:
  Documentation:
    dummy class
  Direct superclasses: standard-object
  Class precedence list: standard-object
  Slots:
    x = 3
    y = 5
  Methods:
    :describe
    :equal
    :eval-inside-yourself
    :flavor
    :fun
    :id
    :init
    :inspect
    :operation-handled-p
    :print-self
    :send-if-handles
    :shared-initialize
    :update-instance-for-different-class
    :which-operations
`, string(out))

	tt.Equal(t, `{
  docs: "dummy class"
  final: false
  inherit: [standard-object]
  methods: [
    ":describe"
    ":equal"
    ":eval-inside-yourself"
    ":flavor"
    ":fun"
    ":id"
    ":init"
    ":inspect"
    ":operation-handled-p"
    ":print-self"
    ":send-if-handles"
    ":shared-initialize"
    ":update-instance-for-different-class"
    ":which-operations"
  ]
  name: dummy
  prototype: null
  slots: {x: 3 y: 5}
}`, pretty.SEN(c.Simplify()))

	tt.Panic(t, func() {
		_ = clos.DefClass("dummy", "", map[string]slip.Object{}, nil, false)
	})
}

func TestClassInstanceInit(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-condition 'error :message "quux") :message)`,
		Expect: `"quux"`,
	}).Test(t)
}

func TestConditionMessageDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method (find-class 'condition) :message out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":message"))
}
