// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"unicode/utf8"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CodeChar{Function: slip.Function{Name: "code-char", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "code-char",
			Args: []*slip.DocArg{
				{Name: "code", Type: "fixnum"},
			},
			Return: "character|nil",
			Text:   `__code-char__ returns the character for _code_ or _nil_ if not a character code.`,
			Examples: []string{
				`(code-char 65) => #\A`,
			},
		}, &slip.CLPkg)
}

// CodeChar represents the code-char function.
type CodeChar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CodeChar) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	var num int
	switch ta := args[0].(type) {
	case slip.Fixnum:
		num = int(ta)
	case slip.Octet:
		num = int(ta)
	default:
		slip.TypePanic(s, depth, "code", args[0], "fixnum", "octet")
	}
	if 0 <= num && num < utf8.MaxRune && utf8.ValidRune(rune(num)) {
		return slip.Character(num)
	}
	return nil
}
