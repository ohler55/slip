// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Subtypep{Function: slip.Function{Name: "subtypep", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "subtypep",
			Args: []*slip.DocArg{
				{
					Name: "type-1",
					Type: "type specifier",
					Text: "A type specifier.",
				},
				{
					Name: "type-2",
					Type: "type specifier",
					Text: "A type specifier.",
				},
			},
			Return: "boolean",
			Text: `__subtypep__ returns _true_ if _type-1_ is a subtype of _type-2_. Type specifiers
are limited to _symbol_ and lists starting with vector or array.`,
			Examples: []string{
				`(subtypep 'fixnum 'integer) => t`,
				"(subtypep 'fixnum 'float) => nil",
			},
		}, &slip.CLPkg)
}

// Subtypep represents the subtypep function.
type Subtypep struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Subtypep) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	result := slip.Values{nil, slip.True}

	pt1, et1 := designatedType(s, args[0], "type-1", depth)
	pt2, et2 := designatedType(s, args[1], "type-2", depth)

	if pt1 != nil && pt2 != nil {
		if (pt1 == pt2 || pt1 != nil && pt1.Inherits(pt2)) &&
			(et2 == nil || et1 == et2 || et1.Inherits(et2)) {
			result[0] = slip.True
		}
	}
	return result
}

func designatedType(s *slip.Scope, arg slip.Object, name string, depth int) (pt slip.Class, et slip.Class) {
	switch ta := arg.(type) {
	case slip.Class:
		pt = ta
	case slip.Symbol:
		pt = slip.FindClass(string(ta))
	case slip.List:
		if len(ta) == 2 {
			pt, _ = designatedType(s, ta[0], name, depth)
			et, _ = designatedType(s, ta[1], name, depth)
		}
	default:
		slip.TypePanic(s, depth, name, ta, "class", "symbol", "list")
	}
	return
}
