// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Eql{Function: slip.Function{Name: "eql", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "eql",
			Args: []*slip.DocArg{
				{Name: "x", Type: "object"},
				{Name: "y", Type: "object"},
			},
			Return: "boolean",
			Text: `__eql__ returns _t_ if _x_ and _y_ are either _eq_ or numbers and have the
same value or characters both are the same character. Otherwise _nil_ is returned.


The common-lisp documentation indicates any _string_ will fail the _eql_ test
as _string_ is excluded from the definition yet the examples provided show a
case sensitive string comparison. This implementation follows the examples and
not the documentation.`,
			Examples: []string{
				"(eql 5 5) => t",
				"(eql 5 5.0) => t",
				`(eql "abc" "abc") => t`,
				`(eql "abc" "ABC") => nil`,
			},
		}, &slip.CLPkg)
}

// Eql represents the eql function.
type Eql struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Eql) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	x := args[1]
	y := args[0]
	if eq(x, y) {
		return slip.True
	}
	switch tx := x.(type) {
	case slip.Character:
		if y.(slip.Character) == tx {
			return slip.True
		}
	case slip.String:
		if x == y {
			return slip.True
		}
	default:
		if same(x, y) != nil {
			return slip.True
		}
	}
	return nil
}
