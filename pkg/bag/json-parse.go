// Copyright (c) 2023, Peter Ohler, All rights reserved.

package bag

import (
	"io"

	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/gi"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := JSONParse{Function: slip.Function{Name: "json-parse", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "json-parse",
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
				{
					Name: "strict",
					Type: "boolean",
					Text: `If true strict JSON parsing is used otherwise SEN
(see https://github.com/ohler55/ojg/blob/develop/sen.md) formatting rules apply.`,
				},
			},
			Return: "nil",
			Text: `__json-parse__ parses _input_ and calls _function_ for each parsed JSON wrapped with
an instance of the _bag-flavor_. If _function_ is a _channel_ then the parsed instance is places on
the channel instead.`,
			Examples: []string{
				`(defvar data '())`,
				`(json-parse (lambda (bag) (setq data cons (send bag :native) data)) "{a:1}{b:2}") => nil`,
				`data => ((("a" . 1)) (("b" . 2)))`,
			},
		}, &Pkg)
}

// JSONParse represents the JSONParse function.
type JSONParse struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *JSONParse) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 4)
	d2 := depth + 1
	var caller slip.Caller
	channel, ok := args[0].(gi.Channel)
	if !ok {
		caller = cl.ResolveToCaller(s, args[0], d2)
	}
	var cb func(any)
	if ok {
		cb = func(j any) {
			inst := flavor.MakeInstance().(*flavors.Instance)
			inst.Any = j
			channel <- inst
		}
	} else {
		cb = func(j any) {
			inst := flavor.MakeInstance().(*flavors.Instance)
			inst.Any = j
			_ = caller.Call(s, slip.List{inst}, d2)
		}
	}
	if 2 < len(args) && args[2] != nil {
		switch ta := args[1].(type) {
		case slip.String:
			oj.MustParse([]byte(ta), cb)
		case slip.Octets:
			oj.MustParse([]byte(ta), cb)
		case io.Reader:
			oj.MustLoad(ta, cb)
		default:
			slip.TypePanic(s, depth, "input", ta, "string", "input-stream")
		}
	} else {
		switch ta := args[1].(type) {
		case slip.String:
			sen.MustParse([]byte(ta), cb)
		case slip.Octets:
			sen.MustParse([]byte(ta), cb)
		case io.Reader:
			sen.MustParseReader(ta, cb)
		default:
			slip.TypePanic(s, depth, "input", ta, "string", "input-stream")
		}
	}
	return nil
}
