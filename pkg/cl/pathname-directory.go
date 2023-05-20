// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PathnameDirectory{Function: slip.Function{Name: "pathname-directory", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "pathname-directory",
			Args: []*slip.DocArg{
				{
					Name: "pathname",
					Type: "string",
					Text: "The pathname to get the directory from.",
				},
				{Name: "&key"},
				{
					Name: "case",
					Type: "keyword",
					Text: "The case keyword value is ignored.",
				},
			},
			Return: "list",
			Text: `__pathname-directory__ returns a list where the first element is either _:absolute_
or _:relative_ which indicated whether the pathname is an absolute path or a relative path. The remainder
of the arguments are the directories in the path excluding the last name.`,
			Examples: []string{
				`(pathname-directory "../one/two/three.lisp") => (:relative :up "one" "two")`,
				`(pathname-directory "/one/two/three.lisp") => (:absolute "one" "two")`,
			},
		}, &slip.CLPkg)
}

// PathnameDirectory represents the pathname-directory function.
type PathnameDirectory struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PathnameDirectory) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 3)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("pathname", args[0], "string")
	}
	var result slip.List
	parts := strings.Split(string(path), "/")
	if 1 < len(parts) {
		if parts[0] == "" {
			result = append(result, slip.Symbol(":absolute"))
			parts = parts[1:]
		} else {
			result = append(result, slip.Symbol(":relative"))
		}
		for _, name := range parts[:len(parts)-1] {
			switch name {
			case "..":
				result = append(result, slip.Symbol(":up"))
			case "":
				// indicates a // which is assumed to be a single / as per common lisp
			default:
				result = append(result, slip.String(name))
			}
		}
	}
	return result
}
