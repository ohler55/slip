// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeRandomState{Function: slip.Function{Name: "make-random-state", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-random-state",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "state",
					Type: "random-state",
					Text: "The random-state to copy, _nil_, or _t_.",
				},
			},
			Return: "nil",
			Text: `__make-random-state__ returns a _random-state_. If _state_ is nil then the new
_random-state_ is a copy of _*random-state*. If _state_ is _t_ then the new _random-state_ is a fresh
_random-state_. If _state_ is _random-state_ then the new _random-state_ is a copy of the _state_.`,
			Examples: []string{
				"*random-state* => #<random-state (3 17 43) (73 49 111)>",
				"(make-random-state nil) => #<random-state (3 17 43) (73 49 111)>",
			},
		}, &slip.CLPkg)
}

// MakeRandomState represents the makeRandomState function.
type MakeRandomState struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeRandomState) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 0, 1)
	var state slip.Object
	if 0 < len(args) {
		state = args[0]
	}
	var rs *RandomState
	if state != slip.True {
		switch ts := state.(type) {
		case nil:
			obj, _ := slip.CLPkg.Get(randomStateStr)
			rs, _ = obj.(*RandomState)
		case *RandomState:
			rs = ts
		default:
			slip.TypePanic(s, depth, "state", state, "random-state")
		}
	}
	return NewRandomState(rs)
}
