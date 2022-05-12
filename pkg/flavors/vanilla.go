// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import "github.com/ohler55/slip"

var vanilla = Flavor{
	name:        "vanilla-flavor",
	docs:        "A Flavor that implements the standard methods.",
	defaultVars: map[string]slip.Object{"self": nil},
	methods: map[string]*method{
		":describe":            {name: ":describe", daemon: describeCaller(true)},
		":id":                  {name: ":id", daemon: idCaller(true)},
		":operation-handler-p": {name: ":operation-handler-p", daemon: hasOpCaller(true)},
		":print-self":          {name: ":print-self", daemon: printCaller(true)},
		":send-if-handles":     {name: ":send-if-handles", daemon: sendIfCaller(true)},
		":which-operations":    {name: ":print-self", daemon: whichOpsCaller(true)},
	},
}

func init() {
	for _, m := range vanilla.methods {
		m.from = &vanilla
	}
}

type describeCaller bool

func (caller describeCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// TBD first arg (len(args)-1) should be self
	return nil
}

type idCaller bool

func (caller idCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// TBD first arg (len(args)-1) should be self
	return nil
}

type hasOpCaller bool

func (caller hasOpCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// TBD first arg (len(args)-1) should be self
	return nil
}

type printCaller bool

func (caller printCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// TBD first arg (len(args)-1) should be self
	return nil
}

type sendIfCaller bool

func (caller sendIfCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// TBD first arg (len(args)-1) should be self
	return nil
}

type whichOpsCaller bool

func (caller whichOpsCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	// TBD first arg (len(args)-1) should be self
	return nil
}
