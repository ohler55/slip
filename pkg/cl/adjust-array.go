// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AdjustArray{Function: slip.Function{Name: "adjust-array", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "adjust-array",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array or vector to adjust.",
				},
				{
					Name: "dimensions",
					Type: "list",
					Text: "The dimensions of the array or vector.",
				},
				{Name: "&key"},
				{
					Name: "element-type",
					Type: "symbol",
					Text: "A type designator (symbol).",
				},
				{
					Name: "initial-element",
					Type: "object",
					Text: "An initial value for if _initial-contents_ is not providd.",
				},
				{
					Name: "initial-contents",
					Type: "list",
					Text: "The initial contents of the array or vector.",
				},
				{
					Name: "fill-pointer",
					Type: "fixnum|t|nil",
					Text: `The initial value for a fill pointer if a _fixnum_ or if _t_ a fill
pointer set to the end of the vector. A value of _nil_ indicates no fill pointer. This option
is only valid for a one dimensional array, a vector.`,
				},
				{
					Name: "displaced-to",
					Type: "array|nil",
					Text: "Not supported",
				},
				{
					Name: "displaced-index-offset",
					Type: "fixnum",
					Text: "Not supported",
				},
			},
			Return: "array",
			Text: `__adjust-array__ adjusts an _array_ or _vector_ with the specified
_dimensions and options.`,
			Examples: []string{
				`(adjust-array (make-array '(2 3)) '(1 2))  => #2A((nil nil))`,
			},
		}, &slip.CLPkg)
}

// AdjustArray represents the adjust-array function.
type AdjustArray struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *AdjustArray) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 14)
	var (
		dims         []int
		initContents slip.List
		fillPtr      int
		elementType  slip.Symbol
	)
	if al, ok := args[0].(slip.ArrayLike); ok {
		elementType = al.ElementType()
		var vl slip.VectorLike
		if vl, ok = al.(slip.VectorLike); ok {
			fillPtr = vl.FillPointer()
		}
	} else {
		slip.PanicType("array", args[0], "array", "vector")
	}
	switch ta := args[1].(type) {
	case slip.Fixnum:
		dims = []int{int(ta)}
	case slip.List:
		for _, v := range ta {
			if num, _ := v.(slip.Fixnum); 0 < num {
				dims = append(dims, int(num))
			} else {
				slip.PanicType("dimensions", args[0], "list of positive fixnums")
			}
		}
	default:
		slip.PanicType("dimensions", ta, "fixnum", "list of positive fixnums")
	}
	rest := args[2:]
	if option, has := slip.GetArgsKeyValue(rest, slip.Symbol(":element-type")); has {
		if sym, ok := option.(slip.Symbol); ok {
			elementType = sym
		} else {
			slip.PanicType(":element-type", option, "symbol")
		}
	}
	initElement, _ := slip.GetArgsKeyValue(rest, slip.Symbol(":initial-element"))
	if option, has := slip.GetArgsKeyValue(rest, slip.Symbol(":initial-contents")); has {
		if list, ok := option.(slip.List); ok {
			initContents = list
		} else {
			slip.PanicType(":initial-contents", option, "list")
		}
	}
	if option, has := slip.GetArgsKeyValue(rest, slip.Symbol(":fill-pointer")); has {
		switch to := option.(type) {
		case nil:
			fillPtr = -1
		case slip.Fixnum:
			fillPtr = int(to)
		default:
			if 0 < len(dims) {
				fillPtr = dims[0]
			}
		}
	}
	switch ta := args[0].(type) {
	case *slip.Array:
		result = ta.Adjust(dims, elementType, initElement, initContents)
	case slip.VectorLike:
		result = ta.Adjust(dims, elementType, initElement, initContents, fillPtr)
	}
	return
}
