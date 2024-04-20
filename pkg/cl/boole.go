// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Boole{Function: slip.Function{Name: "boole", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "boole",
			Args: []*slip.DocArg{
				{
					Name: "op",
					Type: "symbol",
					Text: "The operation to perform.",
				},
				{
					Name: "integer-1",
					Type: "integer",
					Text: "An integer.",
				},
				{
					Name: "integer-2",
					Type: "integer",
					Text: "An integer.",
				},
			},
			Return: "integer",
			Text: `__boole__ performs a bit-wise operation on _integer-1_ and _integer-2_
and returns the result. The operations are:
  __boole-1__     integer-1
  __boole-2__     integer-2
  __boole-and__   _and_
  __boole-andc1__ _and_ complement of integer-1 with integer-2
  __boole-andc2__ _and_ integer-1 with complement of integer-2
  __boole-c1__    complement of integer-1
  __boole-c2__    complement of integer-2
  __boole-clr__   always 0 (all zero bits)
  __boole-eqv__   equivalence (exclusive nor)
  __boole-ior__   inclusive _or_
  __boole-nand__  not-and
  __boole-nor__   not-or
  __boole-orc1__  _or_ complement of integer-1 with integer-2
  __boole-orc2__  _or_ integer-1 with complement of integer-2
  __boole-set__   always -1 (all one bits)
  __boole-xor__   exclusive _or_
`,
			Examples: []string{
				"(boole boole-and 7 2) => 5",
			},
		}, &slip.CLPkg)
}

// Boole represents the boole function.
type Boole struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Boole) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 3, 3)
	if _, ok := args[1].(slip.Integer); !ok {
		slip.PanicType("integer-1", args[1], "integer")
	}
	if _, ok := args[2].(slip.Integer); !ok {
		slip.PanicType("integer-2", args[2], "integer")
	}
	switch args[0] {
	case slip.Symbol("boole-1"):
		result = args[1]
	case slip.Symbol("boole-2"):
		result = args[2]
	case slip.Symbol("boole-and"):
		result = logand(args[1:3])
	case slip.Symbol("boole-andc1"):
		result = logandc1(args[1], args[2])
	case slip.Symbol("boole-andc2"):
		result = logandc1(args[2], args[1])
	case slip.Symbol("boole-c1"):
		result = complement(args[1])
	case slip.Symbol("boole-c2"):
		result = complement(args[2])
	case slip.Symbol("boole-clr"):
		result = slip.Fixnum(0)
	case slip.Symbol("boole-eqv"):
		result = logeqv(args[1:3])
	case slip.Symbol("boole-ior"):
		result = logior(args[1:3])
	case slip.Symbol("boole-nand"):
		result = lognand(args[1], args[2])
	case slip.Symbol("boole-nor"):
		result = lognor(args[1], args[2])
	case slip.Symbol("boole-orc1"):
		result = logorc1(args[1], args[2])
	case slip.Symbol("boole-orc2"):
		result = logorc1(args[2], args[1])
	case slip.Symbol("boole-set"):
		result = slip.Fixnum(-1)
	case slip.Symbol("boole-xor"):
		result = logxor(args[1:3])
	default:
		slip.PanicType("op", args[0],
			"boole-1",
			"boole-2",
			"boole-and",
			"boole-andc1",
			"boole-andc2",
			"boole-c1",
			"boole-c2",
			"boole-clr",
			"boole-eqv",
			"boole-ior",
			"boole-nand",
			"boole-nor",
			"boole-orc1",
			"boole-orc2",
			"boole-set",
			"boole-xor",
		)
	}
	return
}
