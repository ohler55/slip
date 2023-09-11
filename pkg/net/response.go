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
	responseFlavor.DefMethod(":protocol", "", protocolCaller(true))
	responseFlavor.DefMethod(":content-length", "", contentLengthCaller(true))
	responseFlavor.DefMethod(":header", "", headerCaller(true))
	responseFlavor.DefMethod(":header-get", "", headerGetCaller(true))
	responseFlavor.DefMethod(":trailer", "", trailerCaller(true))
	responseFlavor.DefMethod(":trailer-get", "", trailerGetCaller(true))
	responseFlavor.DefMethod(":body", "", bodyCaller(true))
	responseFlavor.DefMethod(":close", "", closeCaller(true))
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
	return `__:status__ => _fixnum_

Returns the status code of the response.
`
}

type protocolCaller bool

func (caller protocolCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":protocol", len(args), "0")
	}
	return slip.String((obj.Any.(*http.Response)).Proto)
}

func (caller protocolCaller) Docs() string {
	return `__:protocol__ => _string_

Returns the protocol of the response.
`
}

type contentLengthCaller bool

func (caller contentLengthCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":content-length", len(args), "0")
	}
	return slip.Fixnum((obj.Any.(*http.Response)).ContentLength)
}

func (caller contentLengthCaller) Docs() string {
	return `__:contentLength__ => _fixnum_

Returns the contentLength of the response.
`
}

type headerCaller bool

func (caller headerCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":header", len(args), "0")
	}
	var header slip.List
	for k, v := range (obj.Any.(*http.Response)).Header {
		a := make(slip.List, len(v)+1)
		a[0] = slip.String(k)
		for i, vs := range v {
			a[i+1] = slip.String(vs)
		}
		header = append(header, a)
	}
	return header
}

func (caller headerCaller) Docs() string {
	return `__:header__ => _assoc-list_

Returns the header of the response as an association list.
`
}

type headerGetCaller bool

func (caller headerGetCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		flavors.PanicMethodArgChoice(obj, ":header-get", len(args), "1")
	}
	key, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("response :header-get key", args[0], "string")
	}
	header := (obj.Any.(*http.Response)).Header
	if value := header.Get(string(key)); 0 < len(value) {
		return slip.String(value)
	}
	return nil
}

func (caller headerGetCaller) Docs() string {
	return `__:header-get__ _key_ => _string_|_nil_
   _key_ for the header value to get

Returns the header of the response as an association list.
`
}

type trailerCaller bool

func (caller trailerCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":trailer", len(args), "0")
	}
	var trailer slip.List
	for k, v := range (obj.Any.(*http.Response)).Trailer {
		a := make(slip.List, len(v)+1)
		a[0] = slip.String(k)
		for i, vs := range v {
			a[i+1] = slip.String(vs)
		}
		trailer = append(trailer, a)
	}
	return trailer
}

func (caller trailerCaller) Docs() string {
	return `__:trailer__ => _assoc-list_

Returns the trailer of the response as an association list.
`
}

type trailerGetCaller bool

func (caller trailerGetCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		flavors.PanicMethodArgChoice(obj, ":trailer-get", len(args), "1")
	}
	key, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("response :trailer-get key", args[0], "string")
	}
	trailer := (obj.Any.(*http.Response)).Trailer
	if value := trailer.Get(string(key)); 0 < len(value) {
		return slip.String(value)
	}
	return nil
}

func (caller trailerGetCaller) Docs() string {
	return `__:trailer__ _key_ => _string_|_nil_
   _key_ for the trailer value to get

Returns the trailer of the response as an association list.
`
}

type bodyCaller bool

func (caller bodyCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":body", len(args), "0")
	}
	return &slip.InputStream{Reader: (obj.Any.(*http.Response)).Body}
}

func (caller bodyCaller) Docs() string {
	return `__:body__ => _input-stream_

Returns the body, an input-stream of the response.
`
}

type closeCaller bool

func (caller closeCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":close", len(args), "0")
	}
	_ = (obj.Any.(*http.Response)).Body.Close()
	return nil
}

func (caller closeCaller) Docs() string {
	return `__:close__ => _nil_

Closes the body of the response.
`
}

// MakeResponse makes a new response.
func MakeResponse(resp *http.Response) (inst *flavors.Instance) {
	inst = responseFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = resp
	return
}
