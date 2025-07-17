// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestBuiltInClassBasic(t *testing.T) {
	scope := slip.NewScope()
	c := slip.ReadString(`(find-class 'fixnum)`, scope).Eval(scope, nil)
	c2 := c
	tt.Equal(t, c, c.Eval(nil, 0))
	tt.Equal(t, true, c.Equal(c2))
	tt.Equal(t, false, c.Equal(nil))
	tt.Equal(t, "[built-in-class class standard-object t]", pretty.SEN(c.Hierarchy()))
	doc := c.(slip.Class).Documentation()
	tt.Equal(t, "built-in fixed number class", doc)
	c.(slip.Class).SetDocumentation("quux")
	tt.Equal(t, "quux", c.(slip.Class).Documentation())
	c.(slip.Class).SetDocumentation(doc)

	tt.Equal(t, "#<built-in-class fixnum>", c.String())
	tt.Equal(t, `{
  docs: "built-in fixed number class"
  inherit: integer
  name: fixnum
  package: common-lisp
  prototype: 42
}`, pretty.SEN(c.Simplify()))
	tt.Panic(t, func() { _ = slip.ReadString(`(make-instance 'fixnum)`, scope).Eval(scope, nil) })

}

func TestBuiltInClassDescribeBasic(t *testing.T) {
	scope := slip.NewScope()
	c := slip.ReadString(`(find-class 'fixnum)`, scope).Eval(scope, nil).(slip.Class)
	out := c.Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `fixnum is a built-in class:
  Documentation:
    built-in fixed number class
  Direct superclasses: integer
  Class precedence list: fixnum integer rational real number t
  Prototype: 42
`, string(out))

	out = c.Describe([]byte{}, 0, 80, true)
	tt.Equal(t, "\x1b[1mfixnum\x1b[m is a built-in class:\n"+
		"  Documentation:\n"+
		"    built-in fixed number class\n"+
		"  Direct superclasses: integer\n"+
		"  Class precedence list: fixnum integer rational real number t\n"+
		"  Prototype: 42\n", string(out))
}

func TestBuiltInClassInherits(t *testing.T) {
	c := slip.FindClass("fixnum")
	tt.Equal(t, true, c.Inherits(slip.FindClass("integer")))
	tt.Equal(t, false, c.Inherits(slip.FindClass("float")))
}

func TestBuiltInClassDefList(t *testing.T) {
	c := slip.FindClass("fixnum")
	tt.Equal(t, 0, len(c.DefList()))
}
