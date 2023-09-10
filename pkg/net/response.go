// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"net/http"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var responseFlavor *flavors.Flavor

func init() {
	responseFlavor = flavors.DefFlavor("response-flavor", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A response is returned from an HTTP client request. It contains
the data associated with the HTTP reply.`),
			},
		},
	)
	responseFlavor.Final = true
	responseFlavor.DefMethod(":status", "", statusCaller(true))
	// TBD other methods
	// - proto
	// - header (assoc list ("content-type" "foo" "bar"))
	// - header-get key
	// - body (input-stream)
	//  - support a close on this or close response instead?
	// - content-length
	// - encoding
	// - trailer
}

type statusCaller bool

func (caller statusCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":status", len(args), "0")
	}
	return slip.Fixnum((obj.Any.(*http.Response)).StatusCode)
}

func (caller statusCaller) Docs() string {
	return `__:status__

Returns the status code of the response.
`
}

// MakeResponse makes a new response.
func MakeResponse(resp *http.Response) (inst *flavors.Instance) {
	inst = responseFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = resp
	return

}
