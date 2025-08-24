// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GraphicCharP{Function: slip.Function{Name: "graphic-char-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "graphic-char-p",
			Args: []*slip.DocArg{
				{
					Name: "character",
					Type: "character",
					Text: "The character to check.",
				},
			},
			Return: "nil",
			Text: `__graphic-char-p__ returns _true_ if _character_ is a graphic character
otherwise nil is returned. If _character_ is not a character an error is raised.`,
			Examples: []string{
				`(graphic-char-p #\A) => t`,
				`(graphic-char-p #\space) => t`,
				`(graphic-char-p #\newline) => nil`,
			},
		}, &slip.CLPkg)
}

// GraphicCharP represents the graphic-char-p function.
type GraphicCharP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GraphicCharP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.TypePanic(s, depth, "character", args[0], "character")
	}
	if unicode.IsGraphic(rune(c)) {
		return slip.True
	}
	return nil
}
