// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
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
always returns _object_. Although not Common LISP standard other conversion are
supported when possible as indicated in the table where the vertical axis the
_object_ type and the horizontal axis the _type_:

                |list
                | |string
                | | |vector
                | | | |octets (bytes)
                | | | | |bit-vector
                | | | | | |character
                | | | | | | |integer
                | | | | | | | |fixnum
                | | | | | | | | |octet (byte)
                | | | | | | | | | |bignum
                | | | | | | | | | | |float
                | | | | | | | | | | | |short-float
                | | | | | | | | | | | | |single-float
                | | | | | | | | | | | | | |double-float
                | | | | | | | | | | | | | | |long-float
                | | | | | | | | | | | | | | | |rational
                | | | | | | | | | | | | | | | | |ratio
                | | | | | | | | | | | | | | | | | |complex
                | | | | | | | | | | | | | | | | | | |symbol
                | | | | | | | | | | | | | | | | | | | |assoc
                | | | | | | | | | | | | | | | | | | | | |hash-table
                | | | | | | | | | | | | | | | | | | | | | |function
                | | | | | | | | | | | | | | | | | | | | | | |signed-byte
                | | | | | | | | | | | | | | | | | | | | | | | |unsigned-byte
  list          |x|x|x|x|x| | | | | | | | | | | | | | | | | | | |
  string        |x|x|x|x| | | | | | | | | | | | | | |x| | |x| | |
  vector        |x|x|x|x|x| | | | | | | | | | | | | | | | | | | |
  octets        |x|x|x|x|x| | | | | | | | | | | | | | | | | | | |
  bit-vector    |x| |x|x|x| |x|x|x|x| | | | | | | | | | | | |x|x|
  character     | | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |x|x|
  integer       | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |x|x|
  fixnum        | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |x|x|
  octet         | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |x|x|
  float         | | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | | | |
  ratio         | | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | | | |
  complex       | | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | | | |
  symbol        |x|x|x|x| | | | | | | | | | | | | | |x| | |x| | |
  assoc         |x| | | | | | | | | | | | | | | | | | |x|x| | | |
  hash-table    | | | | | | | | | | | | | | | | | | | |x|x| | | |
  signed-byte   | | | | |x|x|x|x|x|x| | | | | | | | | | | | |x|x|
  unsigned-byte | | | | |x|x|x|x|x|x| | | | | | | | | | | | |x|x|

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
func (f *Coerce) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)

	return slip.Coerce(args[0], args[1])
}
