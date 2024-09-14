// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Equalp{Function: slip.Function{Name: "equalp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "equalp",
			Args: []*slip.DocArg{
				{Name: "x", Type: "object"},
				{Name: "y", Type: "object"},
			},
			Return: "boolean",
			Text: `__equalp__ returns _t_ if _x_ and _y_ are either _eq_, _eql_, or structurally the same.


Symbols, Numbers, and Characters:
  _equalp_ if _eq_ or _eql_


Cons or List:
  _equalp_ if both have the _equalp_ elements in the same order.


Strings:
  _equalp_ if each element is _equalp_. For _strings_ character comparison is case insensitive.


Arrays:
  __equalp_ if both have the same dimensions and each element is _equalp_.


Hash-Tables:
  _equalp_ if all elements with the same keys as determined by the _hash-table_ :test. Element values must be _equalp_.


Instances:
  _equalp_ if both are of the same flavor and all instance-variables are _equalp_.

`,
			Examples: []string{
				"(equalp 5 5) => t",
				"(equalp 5 5.0) => t",
			},
		}, &slip.CLPkg)
}

// Equalp represents the equalp function.
type Equalp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Equalp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	if equalp(args[0], args[1]) {
		return slip.True
	}
	return nil
}

func equalp(x, y slip.Object) bool {
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
				if !equalp(xv, ty[i]) {
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
	default:
		if x.Equal(y) {
			return true
		}
	}
	return false
}
