// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"

	"github.com/ohler55/ojg/pretty"
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
			Text:   `continues with the rest of the daemon methods.`,
			Examples: []string{
				// TBD defmethod with continue-whopper called in body
				"(continue-whopper fruit :color) => red",
			},
		}, &FlavorsPkg)
}

// ContinueWhopper represents the continueWhopper function.
type ContinueWhopper struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ContinueWhopper) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	fmt.Printf("*** continue-whopper %s --- %v\n", pretty.SEN(s), s.Parent())
	self, _ := s.Get("self").(*Instance)
	loc, _ := s.Get("~whopper-location~").(*whopLoc)
	if self == nil || loc == nil {
		panic("continue-whopper can only be called from a whopper.")
	}
	for ; loc.current < len(loc.methods); loc.current++ {
		wrap := loc.methods[loc.current].wrap
		if wrap == nil {
			continue
		}
		ws := self.Scope.NewScope(nil)
		ws.Let("~whopper-location~", &whopLoc{methods: loc.methods, current: loc.current + 1})

		return wrap.Call(ws, args, depth)
	}
	return self.sendInner(loc.methods, args, depth)
}
