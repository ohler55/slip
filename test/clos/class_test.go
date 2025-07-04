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
	scope := slip.NewScope()
	c := slip.ReadString(`(find-class 'fixnum)`, scope).Eval(scope, nil)
	c2 := c
	tt.Equal(t, c, c.Eval(nil, 0))
	tt.Equal(t, true, c.Equal(c2))
	tt.Equal(t, false, c.Equal(nil))
	tt.Equal(t, "[class standard-object t]", pretty.SEN(c.Hierarchy()))
	doc := c.(slip.Class).Documentation()
	tt.Equal(t, "built-in fixed number class", doc)
	c.(slip.Class).SetDocumentation("quux")
	tt.Equal(t, "quux", c.(slip.Class).Documentation())
	c.(slip.Class).SetDocumentation(doc)

	tt.Equal(t, "#<class fixnum>", c.String())
	tt.Equal(t, `{
  docs: "built-in fixed number class"
  final: true
  inherit: [integer]
  name: fixnum
  prototype: 42
  slots: {}
}`, pretty.SEN(c.Simplify()))
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'fixnum)`, scope).Eval(scope, nil) })

}

func TestClassDescribeBasic(t *testing.T) {
	scope := slip.NewScope()
	c := slip.ReadString(`(find-class 'fixnum)`, scope).Eval(scope, nil).(slip.Class)
	out := c.Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `fixnum is a built-in class:
  Documentation:
    built-in fixed number class
  Direct superclasses: integer
  Class precedence list: integer rational real number built-in-class
  Slots: None
  Prototype: 42
`, string(out))

	out = c.Describe([]byte{}, 0, 80, true)
	tt.Equal(t, "\x1b[1mfixnum\x1b[m is a built-in class:\n"+
		"  Documentation:\n"+
		"    built-in fixed number class\n"+
		"  Direct superclasses: integer\n"+
		"  Class precedence list: integer rational real number built-in-class\n"+
		"  Slots: None\n"+
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
	scope := slip.NewScope()
	found := slip.ReadString(`(find-class 'dummy)`, scope).Eval(scope, nil).(slip.Class)
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
`, string(out))

	tt.Equal(t, `{
  docs: "dummy class"
  final: false
  inherit: [standard-object]
  name: dummy
  prototype: null
  slots: {x: 3 y: 5}
}`, pretty.SEN(c.Simplify()))

	tt.Panic(t, func() {
		_ = clos.DefClass("dummy", "", map[string]slip.Object{}, nil, false)
	})
	c.SetNoMake(true)
	tt.Equal(t, true, c.NoMake())
}

func TestClassInherits(t *testing.T) {
	c := slip.FindClass("fixnum")
	tt.Equal(t, true, c.Inherits(slip.FindClass("integer")))
	tt.Equal(t, false, c.Inherits(slip.FindClass("float")))
}

func TestClassDefList(t *testing.T) {
	c := slip.FindClass("fixnum")
	tt.Equal(t, 0, len(c.DefList()))
}
