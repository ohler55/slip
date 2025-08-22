// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defListAllClasses() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ListAllClasses{Function: slip.Function{Name: "list-all-classes", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "list-all-classes",
			Args: []*slip.DocArg{
				{Name: "&key"},
				{
					Name: "metaclass",
					Type: "symbol",
					Text: "The metaclass of the classes to return on a list.",
				},
				{
					Name: "package",
					Type: "package|symbol|string",
					Text: "The package to to get the class list from. (default: __*package*__).",
				},
			},
			Return: "list",
			Text: `__list-all-classes__ returns a list of all classes. If _metaclass_ is provided only
classes with the specified metaclass are returned.`,
			Examples: []string{
				"(list-all-classes :metaclass 'flavor) => (bag-flavor vanilla-flavor)",
			},
		}, &Pkg)
}

// ListAllClasses represents the list-all-classes function.
type ListAllClasses struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ListAllClasses) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 4)
	p := slip.CurrentPackage
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":package")); has {
		switch tv := v.(type) {
		case *slip.Package:
			p = tv
		case slip.Symbol:
			if p = slip.FindPackage(string(tv)); p == nil {
				slip.NewPanic("%s does not name a package.", tv)
			}
		case slip.String:
			if p = slip.FindPackage(string(tv)); p == nil {
				slip.NewPanic("%s does not name a package.", tv)
			}
		default:
			slip.TypePanic(s, depth, ":package", v, "symbol", "string", "package")
		}
	}
	var meta slip.Object
	if v, has := slip.GetArgsKeyValue(args, slip.Symbol(":metaclass")); has {
		meta = v
	}
	var list slip.List
	p.EachClass(func(c slip.Class) {
		if meta == nil || meta == c.Metaclass() {
			list = append(list, c)
		}
	})
	return list
}
