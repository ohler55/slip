// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ContinueWhopper{Function: slip.Function{Name: "continue-whopper", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "continue-whopper",
			Args: []*slip.DocArg{
				{Name: slip.AmpRest},
				{
					Name: "args",
					Type: "object",
					Text: `The remaining arguments to the method.`,
				},
			},
			Return: "object",
			Text:   `__continue-whopper__ continues with the rest of the daemon methods using the arguments provided.`,
			Examples: []string{
				"(defwhopper (blueberry :rot) () (continue-whopper))",
			},
		}, &Pkg)
}

// ContinueWhopper represents the continueWhopper function.
type ContinueWhopper struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ContinueWhopper) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self, _ := s.Get("self").(*Instance)
	loc, _ := s.Get("~whopper-location~").(*whopLoc)
	for loc.current++; loc.current < len(loc.methods); loc.current++ {
		wrap := loc.methods[loc.current].wrap
		if wrap == nil {
			continue
		}
		ws := self.Scope.NewScope()
		ws.Let("~whopper-location~", &whopLoc{methods: loc.methods, current: loc.current + 1})
	whopper:
		switch tw := wrap.(type) {
		case *slip.Lambda:
			(wrap.(*slip.Lambda)).Closure = ws
		case *Daemon:
			wrap = tw.caller
			goto whopper
		}
		return wrap.Call(ws, args, depth)
	}
	return self.innerReceive(s, loc.methods, args, depth)
}
