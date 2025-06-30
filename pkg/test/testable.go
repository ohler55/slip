// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var testableFlavor *flavors.Flavor

// TestableFlavor returns the testable-flavor.
func TestableFlavor() *flavors.Flavor {
	if testableFlavor == nil {
		Pkg.Initialize(nil)
		testableFlavor = flavors.DefFlavor("testable-flavor",
			map[string]slip.Object{ // variables
				"name":   nil,
				"parent": nil,
			},
			nil, // inherited
			slip.List{ // options
				slip.List{
					slip.Symbol(":documentation"),
					slip.String(`The testable-flavor is an abstract flavor used by suites and tests.`),
				},
				slip.Symbol(":gettable-instance-variables"),
				slip.List{
					slip.Symbol(":settable-instance-variables"),
					slip.Symbol("name"),
				},
				slip.List{
					slip.Symbol(":inittable-instance-variables"),
					slip.Symbol("name"),
					slip.Symbol("parent"),
				},
			},
			&Pkg,
		)
		testableFlavor.DefMethod(":init", "", initCaller(true))
	}
	return testableFlavor
}

type initCaller bool

func (caller initCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if parent, _ := s.Get("parent").(*flavors.Instance); parent != nil {
		if parent.Type != suiteFlavor {
			panic(fmt.Sprintf("a suite or test parent must be an instance of suite-flavor or nil, not %s", parent))
		}
		children, _ := parent.Get("children").(slip.List)
		children = append(children, self)
		parent.Set("children", children)
	}
	return nil
}

func (caller initCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":init",
		Text: "Sets the initial value when _make-instance_ is called.",
	}
}

func getRunKeys(args slip.List) (filter slip.List, verbose, trace bool) {
	for pos := 0; pos < len(args); pos += 2 {
		sym, ok := args[pos].(slip.Symbol)
		if !ok {
			slip.PanicType("keyword", args[pos], "keyword")
		}
		if len(args)-1 <= pos {
			panic(fmt.Sprintf("%s missing an argument", sym))
		}
		switch strings.ToLower(string(sym)) {
		case ":filter":
			filter, _ = args[pos+1].(slip.List)
		case ":verbose":
			verbose = args[pos+1] != nil
		case ":trace":
			trace = args[pos+1] != nil
		default:
			slip.PanicType("keyword", sym, ":filter", ":verbose", "trace")
		}
	}
	return
}
