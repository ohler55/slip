// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos_test

import (
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestClassBasic(t *testing.T) {
	c := slip.ReadString(`(find-class 'fixnum)`).Eval(slip.NewScope(), nil)
	tt.Equal(t, c, c.Eval(nil, 0))
	tt.Equal(t, true, c.Equal(c))
	tt.Equal(t, false, c.Equal(nil))
	tt.Equal(t, "[class standard-object t]", pretty.SEN(c.Hierarchy()))
	tt.Equal(t, "built-in fixed number class", c.(slip.Class).Documentation())
	tt.Equal(t, `{
  docs: "built-in fixed number class"
  final: true
  inherit: [built-in-class integer rational real number]
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
  Class precedence list: built-in-class integer rational real number
  Slots: None
  Methods: None
  Prototype: 42
`, string(out))
}
