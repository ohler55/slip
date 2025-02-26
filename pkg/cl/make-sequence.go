// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeSequence{Function: slip.Function{Name: "make-sequence", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-sequence",
			Args: []*slip.DocArg{
				{
					Name: "result-type",
					Type: "symbol|list",
					Text: "The sequence type specifier.",
				},
				{
					Name: "size",
					Type: "fixnum",
					Text: "A non-negative fixnum.",
				},
				{Name: "&key"},
				{
					Name: "initial-element",
					Type: "object",
					Text: `The value to use as the initial element of the sequence.`,
				},
			},
			Return: "sequence",
			Text: `__make-sequence__ returns sequence of type _result-type_ with each element set to
_initial-element_.`,
			Examples: []string{
				"(make-sequence 'list 3 :initial-element 'x) => (x x x)",
			},
		}, &slip.CLPkg)
}

// MakeSequence represents the make-sequence function.
type MakeSequence struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeSequence) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 4)
	var element slip.Object
	if v, ok := slip.GetArgsKeyValue(args[2:], slip.Symbol(":initial-element")); ok {
		element = v
	}
	size := getFixnumArg(args[1], "size")
	switch rt := args[0].(type) {
	case slip.Symbol:
		switch rt {
		case slip.Symbol("list"):
			result = f.makeList(size, element)
		case slip.Symbol("string"):
			result = f.makeString(size, element, 2 < len(args))
		case slip.Symbol("octets"):
			result = f.makeOctets(size, element, 2 < len(args))
		case slip.Symbol("vector"):
			result = f.makeVector(size, element, slip.TrueSymbol)
		default:
			slip.PanicType("result-type", rt, "list", "string", "vector", "octets")
		}
	case slip.List:
		if len(rt) != 2 || rt[0] != slip.Symbol("vector") {
			slip.PanicType("result-type", rt, "sequence type-designator")
		}
		result = f.makeVector(size, element, rt[1])
	default:
		slip.PanicType("result-type", rt, "sequence type-designator")
	}
	return
}

func (f *MakeSequence) makeList(size int, element slip.Object) slip.Object {
	list := make(slip.List, size)
	for i := 0; i < size; i++ {
		list[i] = element
	}
	return list
}

func (f *MakeSequence) makeString(size int, element slip.Object, elementSet bool) slip.Object {
	var r rune
	if elementSet {
		c, ok := element.(slip.Character)
		if !ok {
			slip.PanicType(":initial-value", element, "character")
		}
		r = rune(c)
	}
	seq := make([]rune, size)
	for i := 0; i < size; i++ {
		seq[i] = r
	}
	return slip.String(seq)
}

func (f *MakeSequence) makeOctets(size int, element slip.Object, elementSet bool) slip.Object {
	var b byte
	if elementSet {
		b = byte(ToOctet(element).(slip.Octet))
	}
	seq := make(slip.Octets, size)
	for i := 0; i < size; i++ {
		seq[i] = b
	}
	return seq
}

func (f *MakeSequence) makeVector(size int, element slip.Object, elementType slip.Object) slip.Object {
	et, ok := elementType.(slip.Symbol)
	if !ok {
		slip.PanicType("element-type", elementType, "symbol")
	}
	return slip.NewVector(size, et, element, nil, false)
}
