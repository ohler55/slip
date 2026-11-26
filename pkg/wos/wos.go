// Copyright (c) 2022, Peter Ohler, All rights reserved.

package wos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/generic"
)

var flavor *flavors.Flavor

func defWos() *flavors.Flavor {
	Pkg.Initialize(nil)
	flavor = flavors.DefFlavor("wos-flavor", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A wos is TBD`),
			},
		},
		&Pkg,
	)
	generic.DefClassMethod(flavor, ":init", "", initCaller{})
	// TBD other methods

	return flavor
}

// Flavor returns the wos-flavor.
func Flavor() *flavors.Flavor {
	return flavor
}

type initCaller struct{}

func (caller initCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, self, ":init", len(args), "0")
	}
	self.Any = nil
	// TBD set self.Any to Wos struct

	return nil
}

func (caller initCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":init",
		Text: "Initialized the instance when _make-instance_ is called.",
	}
}
