// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeArray{Function: slip.Function{Name: "make-array", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-array",
			Args: []*slip.DocArg{
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
					Name: "adjustable",
					Type: "boolean",
					Text: "If true the array or vector will be expressly adjustable.",
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
			Text: `__make-array__ makes a new _array_ or _vector_ with the specified
_dimensions and options. A one dimensional array is a _vector_.`,
			Examples: []string{
				`(make-array '(2 3)) => #2A((nil nil nil) (nil nil nil))`,
			},
		}, &slip.CLPkg)
}

// MakeArray represents the make-array function.
type MakeArray struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeArray) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 15)
	var (
		dims         []int
		initContents slip.List
	)
	adjustable := true
	fillPtr := -1
	elementType := slip.TrueSymbol
	switch ta := args[0].(type) {
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
	rest := args[1:]
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
	if option, has := slip.GetArgsKeyValue(rest, slip.Symbol(":adjustable")); has {
		adjustable = option != nil
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
	if len(dims) == 1 {
		switch elementType {
		case slip.OctetSymbol:
			if fillPtr < 0 {
				v := make(slip.Octets, dims[0])
				if initElement != nil {
					initElement = slip.Coerce(initElement, slip.OctetSymbol)
					if o, ok := initElement.(slip.Octet); ok {
						for i := len(v) - 1; 0 <= i; i-- {
							v[i] = byte(o)
						}
					} else {
						slip.PanicType(":initial-element", initElement, "octet")
					}
				}
				for i, c := range initContents {
					o, _ := slip.Coerce(c, slip.OctetSymbol).(slip.Octet)
					v[i] = byte(o)
				}
				return v
			}
		case slip.BitSymbol:
			bv := slip.BitVector{CanAdjust: adjustable}
			return bv.Adjust(dims, slip.BitSymbol, initElement, initContents, fillPtr)
		}
		v := slip.NewVector(dims[0], elementType, initElement, initContents, adjustable)
		v.FillPtr = fillPtr

		return v
	}
	return slip.NewArray(dims, elementType, initElement, initContents, adjustable)
}
