// Copyright (c) 2025, Peter Ohler, All rights reserved.

package bag

import (
	"io"

	"github.com/ohler55/ojg/discover"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/gi"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DiscoverJSON{Function: slip.Function{Name: "discover-json", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "discover-json",
			Args: []*slip.DocArg{
				{
					Name: "function",
					Type: "symbol|lambda|channel",
					Text: "The function to call for each JSON in _input_ or a _channel_ to put a _bag_ instance on.",
				},
				{
					Name: "input",
					Type: "input-stream|string|octets",
					Text: "The _input-stream_, _string_, or _octets_ to read from.",
				},
				{Name: "&optional"},
				{
					Name: "strict",
					Type: "boolean",
					Text: `If true strict JSON parsing is used otherwise SEN
(see https://github.com/ohler55/ojg/blob/develop/sen.md) formatting rules apply.`,
				},
			},
			Return: "nil",
			Text: `__discover-json__ attempts to discover JSON or SEN in the _input_ and calls
_function_ for each parsed JSON or SEN wrapped with an instance of the _bag-flavor_. If _function_
is a _channel_ then the parsed instance is placed on the channel instead.`,
			Examples: []string{
				`(defvar data nil)`,
				`(discover-json`,
				`  (lambda (bag) (setq data cons (send bag :native) data))`,
				`  "first {a:1} second {b:2}") => nil`,
				`data => ((("a" . 1)) (("b" . 2)))`,
			},
		}, &Pkg)
}

// DiscoverJSON represents the DiscoverJSON function.
type DiscoverJSON struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *DiscoverJSON) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 3)
	d2 := depth + 1
	var caller slip.Caller
	channel, ok := args[0].(gi.Channel)
	if !ok {
		caller = cl.ResolveToCaller(s, args[0], d2)
	}
	var cb func(any) bool
	if ok {
		cb = func(j any) bool {
			inst := flavor.MakeInstance().(*flavors.Instance)
			inst.Any = j
			channel <- inst
			return false
		}
	} else {
		cb = func(j any) bool {
			inst := flavor.MakeInstance().(*flavors.Instance)
			inst.Any = j
			return caller.Call(s, slip.List{inst}, d2) != nil

		}
	}
	if 2 < len(args) && args[2] != nil {
		switch ta := args[1].(type) {
		case slip.String:
			discover.JSON([]byte(ta), cb)
		case slip.Octets:
			discover.JSON([]byte(ta), cb)
		case io.Reader:
			discover.ReadJSON(ta, cb)
		default:
			slip.TypePanic(s, depth, "input", ta, "string", "input-stream")
		}
	} else {
		switch ta := args[1].(type) {
		case slip.String:
			discover.SEN([]byte(ta), cb)
		case slip.Octets:
			discover.SEN([]byte(ta), cb)
		case io.Reader:
			discover.ReadSEN(ta, cb)
		default:
			slip.TypePanic(s, depth, "input", ta, "string", "input-stream")
		}
	}
	return nil
}
