// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NameChar{Function: slip.Function{Name: "name-char", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "name-char",
			Args: []*slip.DocArg{
				{Name: "name", Type: "string|symbol"},
			},
			Return: "string",
			Text:   `__name-char__ returns the character for _name_.`,
			Examples: []string{
				`(name-char "space") => #\Space`,
				`(name-char 'space) => #\Space`,
			},
		}, &slip.CLPkg)
}

// NameChar represents the name-char function.
type NameChar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *NameChar) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	var name string
	switch ta := args[0].(type) {
	case slip.Symbol:
		name = string(ta)
	case slip.String:
		name = string(ta)
	default:
		slip.TypePanic(s, depth, "name", args[0], "string", "symbol")
	}
	if strings.HasPrefix(name, `#\`) {
		ra := []rune(name)
		if len(ra) == 3 {
			return slip.Character(ra[2])
		}
	} else {
		for i, n := range charNames {
			if strings.EqualFold(name, string(n)) {
				return slip.Character(i)
			}
		}
	}
	return nil
}
