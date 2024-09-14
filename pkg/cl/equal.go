// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Equal{Function: slip.Function{Name: "equal", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "equal",
			Args: []*slip.DocArg{
				{Name: "x", Type: "object"},
				{Name: "y", Type: "object"},
			},
			Return: "boolean",
			Text: `__equal__ returns _t_ if _x_ and _y_ are either _eq_, _eql_, or structurally the same.


Symbols, Numbers, and Characters:
  _equal_ if _eq_ or _eql_


Cons or List:
  _equal_ if both have the _equal_ elements in the same order.


Strings and Vectors:
  _equal_ if each element is _equal_. For _strings_ character comparison is case insensitive.


Others (Hash-Tables, Instances, ...):
  _equal_ if _eq_.

`,
			Examples: []string{
				"(equal 5 5) => t",
				"(equal 5 5.0) => t",
			},
		}, &slip.CLPkg)
}

// Equal represents the equal function.
type Equal struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Equal) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	if equal(args[0], args[1]) {
		return slip.True
	}
	return nil
}

func equal(x, y slip.Object) bool {
	if eq(x, y) {
		return true
	}
	switch tx := x.(type) {
	case slip.Character:
		if y.(slip.Character) == tx {
			return true
		}
	case slip.Number:
		if same(x, y) != nil {
			return true
		}
	case slip.String:
		if ty, ok := y.(slip.String); ok && strings.EqualFold(string(tx), string(ty)) {
			return true
		}
	case slip.List:
		if ty, ok := y.(slip.List); ok && len(tx) == len(ty) {
			for i, xv := range tx {
				if !equal(xv, ty[i]) {
					return false
				}
			}
			return true
		}
	case *slip.Vector, slip.Octets:
		return tx.Equal(y)
	case slip.Tail:
		if ty, ok := y.(slip.Tail); ok {
			return equal(tx.Value, ty.Value)
		}
	}
	return false
}
