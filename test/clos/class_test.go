// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/clos"
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
  inherit: [built-in-class integer]
  methods: []
  name: fixnum
  prototype: 42
  slots: {}
}`, pretty.SEN(c.Simplify()))
	tt.Panic(t, func() { _ = c.(slip.Class).MakeInstance() })
}

func TestClassDescribeBasic(t *testing.T) {
	c := slip.ReadString(`(find-class 'fixnum)`).Eval(slip.NewScope(), nil).(slip.Class)
	out := c.Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `fixnum is a built-in class:
  Documentation:
    built-in fixed number class
  Direct superclasses: built-in-class integer
  Class precedence list: built-in-class integer rational real number
  Slots: None
  Methods: None
  Prototype: 42
`, string(out))

	out = c.Describe([]byte{}, 0, 80, true)
	tt.Equal(t, "\x1b[1mfixnum\x1b[m is a built-in class:\n"+
		"  Documentation:\n"+
		"    built-in fixed number class\n"+
		"  Direct superclasses: built-in-class integer\n"+
		"  Class precedence list: built-in-class integer rational real number\n"+
		"  Slots: None\n"+
		"  Methods: None\n"+
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
	c.DefMethod("fun")
	found := slip.ReadString(`(find-class 'dummy)`).Eval(slip.NewScope(), nil).(slip.Class)
	tt.Equal(t, c, found)
	out := c.Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `dummy is a class:
  Documentation:
    dummy class
  Direct superclasses:
  Class precedence list:
  Slots:
    x = 3
    y = 5
  Methods:
    fun
`, string(out))

	tt.Equal(t, `{
  docs: "dummy class"
  final: false
  inherit: []
  methods: [fun]
  name: dummy
  prototype: null
  slots: {x: 3 y: 5}
}`, pretty.SEN(c.Simplify()))

	tt.Panic(t, func() {
		_ = clos.DefClass("dummy", "", map[string]slip.Object{}, nil, false)
	})
}
