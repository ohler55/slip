// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Coerce{Function: slip.Function{Name: "coerce", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "coerce",
			Args: []*slip.DocArg{
				{Name: "object", Type: "object"},
				{
					Name: "type",
					Type: "symbol",
					Text: `A type specifier for the result.`,
				},
			},
			Return: "object",
			Text: `__coerce__ returns _object_ converted to _type_. A _type_ of _t_
always return _object_. Although not Common LISP standard other conversion are
supported when possible as indicated in the table where the vertical axis the
_object_ type and the horizontal axis the the _type_:

             |list
             | |string
             | | |vector
             | | | |character
             | | | | |integer
             | | | | | |fixnum
             | | | | | | |bignum
             | | | | | | | |float
             | | | | | | | | |short-float
             | | | | | | | | | |single-float
             | | | | | | | | | | |double-float
             | | | | | | | | | | | |long-float
             | | | | | | | | | | | | |rational
             | | | | | | | | | | | | | |ratio
             | | | | | | | | | | | | | | |complex
             | | | | | | | | | | | | | | | |symbol
             | | | | | | | | | | | | | | | | |assoc
             | | | | | | | | | | | | | | | | | |hash-table
             | | | | | | | | | | | | | | | | | | |function
  list       |x|x|x| | | | | | | | | | | | | | | | |
  string     |x|x|x| | | | | | | | | | | | | | | |x|
  vector     |x|x|x| | | | | | | | | | | | | | | | |
  character  | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | |
  integer    | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | |
  float      | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | |
  ratio      | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | |
  complex    | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | |
  symbol     |x|x|x| | | | | | | | | | | | | | | |x|
  assoc      |x| | | | | | | | | | | | | | | |x|x| |
  hash-table | | | | | | | | | | | | | | | | |x|x| |

`,
			Examples: []string{
				"(coerce 4 'float) => 4.0",
			},
		}, &slip.CLPkg)
}

// Coerce represents the coerce function.
type Coerce struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Coerce) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	switch args[1] {
	case slip.True:
		result = args[0]
	case slip.Symbol("list"):
		result = f.toList(args[0])
	case slip.Symbol("string"):
		result = f.toString(args[0])
	case slip.Symbol("vector"):
		result = f.toVector(args[0])
	case slip.Symbol("character"):
		// TBD
	case slip.Symbol("integer"):
		// TBD
	case slip.Symbol("fixnum"):
		// TBD
	case slip.Symbol("bignum"):
		// TBD
	case slip.Symbol("float"):
		// TBD
	case slip.Symbol("short-float"), slip.Symbol("single-float"):
		// TBD
	case slip.Symbol("double-float"):
		// TBD
	case slip.Symbol("long-float"):
		// TBD
	case slip.Symbol("rational"):
		// TBD
	case slip.Symbol("ratio"):
		// TBD
	case slip.Symbol("complex"):
		// TBD
	case slip.Symbol("symbol"):
		// TBD
	case slip.Symbol("assoc"):
		// TBD
	case slip.Symbol("hash-table"):
		// TBD
	case slip.Symbol("function"):
		// TBD
	default:
		panic(fmt.Sprintf("%s is not a valid coerce result type", args[1]))
	}
	return
}

func (f *Coerce) toList(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case nil, slip.List:
		result = ta
	case slip.String:
		ra := []rune(ta)
		list := make(slip.List, len(ra))
		for i, r := range ra {
			list[i] = slip.Character(r)
		}
		result = list
	case slip.Symbol:
		ra := []rune(ta)
		list := make(slip.List, len(ra))
		for i, r := range ra {
			list[i] = slip.Character(r)
		}
		result = list
	case slip.Vector:
		result = slip.List(ta)
	default:
		f.panicNotPossible(ta, "list")
	}
	return
}

func (f *Coerce) toVector(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case nil, slip.Vector:
		result = ta
	case slip.String:
		ra := []rune(ta)
		vector := make(slip.Vector, len(ra))
		for i, r := range ra {
			vector[i] = slip.Character(r)
		}
		result = vector
	case slip.Symbol:
		ra := []rune(ta)
		vector := make(slip.Vector, len(ra))
		for i, r := range ra {
			vector[i] = slip.Character(r)
		}
		result = vector
	case slip.List:
		result = slip.Vector(ta)
	default:
		f.panicNotPossible(ta, "vector")
	}
	return
}

func (f *Coerce) toString(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case nil:
		result = slip.String("")
	case slip.List:
		result = f.listToString(ta)
	case slip.String:
		result = ta
	case slip.Symbol:
		result = slip.String(ta)
	case slip.Vector:
		result = f.listToString(slip.List(ta))
	default:
		f.panicNotPossible(ta, "string")
	}
	return
}

func (f *Coerce) listToString(list slip.List) slip.Object {
	ra := make([]rune, len(list))
	for i, v := range list {
		if r, ok := v.(slip.Character); ok {
			ra[i] = rune(r)
		} else {
			f.panicNotPossible(list, "string")
		}
	}
	return slip.String(ra)
}

func (f *Coerce) panicNotPossible(arg slip.Object, rtype string) {
	if arg == nil {
		panic(fmt.Sprintf("Can not coerce a nil into a %s", rtype))
	}
	panic(fmt.Sprintf("Can not coerce a %s into a %s", arg.Hierarchy()[0], rtype))
}
