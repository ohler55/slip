// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DigitChar{Function: slip.Function{Name: "digit-char", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "digit-char",
			Args: []*slip.DocArg{
				{
					Name: "weight",
					Type: "integer",
					Text: "The value less than _radix_.",
				},
				{
					Name: "radix",
					Type: "integer",
					Text: "The radix for the _weight_. The default is 10.",
				},
			},
			Return: "character",
			Text: `__digit-char__ returns the _character_ used to represent the _weight_ based on _radix_.
_nil_ is returned if _weight_ is greater than or equal to _radix_.`,
			Examples: []string{
				`(digit-char 7) => #\7`,
				`(digit-char 7 6) => nil`,
				`(digit-char 14 16) => #\E`,
			},
		}, &slip.CLPkg)
}

// DigitChar represents the digit-char function.
type DigitChar struct {
	slip.Function
}

const baseChars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

// Call the function with the arguments provided.
func (f *DigitChar) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 2)
	radix := 10
	var weight int
	if w, ok := args[0].(slip.Fixnum); ok && 0 <= w {
		weight = int(w)
	} else {
		slip.PanicType("weight", args[0], "unsigned integer")
	}
	if 1 < len(args) {
		if r, ok := args[1].(slip.Fixnum); ok && 0 < r && r <= 36 {
			radix = int(r)
		} else {
			slip.PanicType("radix", args[1], "integer less than or equal to 36")
		}
	}
	if radix <= weight {
		return nil
	}
	return slip.Character(rune(baseChars[weight]))
}
