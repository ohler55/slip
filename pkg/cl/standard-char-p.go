// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

// . 0123456789abcdef0123456789abcdef
const standardChars = "" +
	"..........o....................." + // 0x00
	"oooooooooooooooooooooooooooooooo" + // 0x20
	"oooooooooooooooooooooooooooooooo" + // 0x40
	"ooooooooooooooooooooooooooooooo." //   0x60

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StandardCharP{Function: slip.Function{Name: "standard-char-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "standard-char-p",
			Args: []*slip.DocArg{
				{
					Name: "character",
					Type: "character",
					Text: "The character to check.",
				},
			},
			Return: "nil",
			Text: `__standard-char-p__ returns _true_ if _character_ is an standardbetic character
otherwise nil is returned. If _character_ is not a character an error is raised.`,
			Examples: []string{
				`(standard-char-p #\A) => t`,
				`(standard-char-p #\1) => nil`,
			},
		}, &slip.CLPkg)
}

// StandardCharP represents the standard-char-p function.
type StandardCharP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StandardCharP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.PanicType("character", args[0], "character")
	}
	if c < 0x007f && standardChars[c] == 'o' {
		return slip.True
	}
	return nil
}
