// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import "github.com/ohler55/slip"

var vanilla = Flavor{
	name:        "vanilla-flavor",
	docs:        "A Flavor that implements the standard methods.",
	defaultVars: map[string]slip.Object{"self": nil},
	methods: map[string]*method{
		":describe":            {name: ":describe", primary: describeCaller(true)},
		":id":                  {name: ":id", primary: idCaller(true)},
		":operation-handler-p": {name: ":operation-handler-p", primary: hasOpCaller(true)},
		":print-self":          {name: ":print-self", primary: printCaller(true)},
		":send-if-handles":     {name: ":send-if-handles", primary: sendIfCaller(true)},
		":which-operations":    {name: ":print-self", primary: whichOpsCaller(true)},
	},
}

func init() {
	for _, m := range vanilla.methods {
		m.from = &vanilla
	}
	FlavorsPkg.Set(vanilla.name, &vanilla)
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
