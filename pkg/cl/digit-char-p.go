// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DigitCharP{Function: slip.Function{Name: "digit-char-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "digit-char-p",
			Args: []*slip.DocArg{
				{
					Name: "char",
					Type: "character",
					Text: "The character to return the weight of.",
				},
				{
					Name: "radix",
					Type: "integer",
					Text: "The radix for the _weight_. The default is 10.",
				},
			},
			Return: "nil",
			Text: `__digit-char-p__ returns the weight of the _char_ based on _radix_.
_nil_ is returned if _char_ is not a character for _radix_.`,
			Examples: []string{
				`(digit-char-p #A 16) => 10`,
				`(digit-char-p #\7 6) => nil`,
			},
		}, &slip.CLPkg)
}

// DigitCharP represents the digit-char-p function.
type DigitCharP struct {
	slip.Function
}

// Character weights start at '0' and fill to 'z'. Negative numbers indicate
// no weight.
var charWeights = []int{
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, -1, -1, -1, -1, -1, -1,
	-1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
	-1, -1, -1, -1, -1,
	-1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
}

// Call the function with the arguments provided.
func (f *DigitCharP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 2)
	radix := 10
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.PanicType("char", args[0], "character")
	}
	if 1 < len(args) {
		if r, ok := args[1].(slip.Fixnum); ok && 0 < r && r <= 36 {
			radix = int(r)
		} else {
			slip.PanicType("radix", args[1], "integer less than or equal to 36")
		}
	}
	if c < '0' || 'z' < c {
		return nil
	}
	weight := charWeights[int(c-'0')]
	if radix <= weight {
		return nil
	}
	return slip.Fixnum(weight)
}
