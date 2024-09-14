// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Svref{Function: slip.Function{Name: "svref", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "svref",
			Args: []*slip.DocArg{
				{
					Name: "simple-vector",
					Type: "simple-vector",
					Text: "The vector to get a value from.",
				},
				{
					Name: "index",
					Type: "fixnum",
					Text: "The index into the vector.",
				},
			},
			Return: "object",
			Text:   `__svref__ returns the indexed element of _vector_.`,
			Examples: []string{
				"(svref (make-array 4 :initial-contents '(a b c d)) 2) => c",
			},
		}, &slip.CLPkg)
}

// Svref represents the svref function.
type Svref struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Svref) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	index, ok := args[1].(slip.Fixnum)
	if !ok {
		slip.PanicType("index", args[1], "fixnum")
	}
	switch ta := args[0].(type) {
	case slip.Octets:
		if len(ta) <= int(index) || index < 0 {
			slip.NewPanic("Invalid index %s. Should be between 0 and %d.", index, len(ta))
		}
		result = slip.Octet(ta[index])
	case *slip.Vector:
		if ta.FillPtr == -1 && ta.ElementType() == slip.TrueSymbol {
			result = ta.Get(int(index))
		} else {
			slip.PanicType("simple-vector", args[0], "simple-vector")
		}
	default:
		slip.PanicType("simple-vector", args[0], "simple-vector")
	}
	return
}

// Place a value in the first position of a list or cons.
func (f *Svref) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	index, ok := args[1].(slip.Fixnum)
	if !ok {
		slip.PanicType("index", args[1], "fixnum")
	}
	switch ta := args[0].(type) {
	case slip.Octets:
		if len(ta) <= int(index) || index < 0 {
			slip.NewPanic("Invalid index %s. Should be between 0 and %d.", args[1:], len(ta))
		}
		var o slip.Octet
		if o, ok = value.(slip.Octet); ok {
			ta[index] = byte(o)
		} else {
			slip.PanicType("value", value, "octet")
		}
	case *slip.Vector:
		if ta.FillPtr == -1 && ta.ElementType() == slip.TrueSymbol {
			ta.Set(value, int(index))
		} else {
			slip.PanicType("simple-vector", args[0], "simple-vector")
		}
	default:
		slip.PanicType("simple-vector", args[0], "simple-vector")
	}
}
