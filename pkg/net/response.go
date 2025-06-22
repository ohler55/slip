// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"io"
	"net/http"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var responseFlavor *flavors.Flavor

func defResponse() *flavors.Flavor {
	Pkg.Initialize(nil)
	responseFlavor = flavors.DefFlavor("http-response-flavor", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A response is returned from an HTTP client request. It contains
the data associated with the HTTP reply.`),
			},
		},
		&Pkg,
	)
	responseFlavor.Final = true
	responseFlavor.DefMethod(":status", "", respStatusCaller(true))
	responseFlavor.DefMethod(":protocol", "", respProtocolCaller(true))
	responseFlavor.DefMethod(":content", "", respContentCaller(true))
	responseFlavor.DefMethod(":content-length", "", respContentLengthCaller(true))
	responseFlavor.DefMethod(":header", "", respHeaderCaller(true))
	responseFlavor.DefMethod(":header-get", "", respHeaderGetCaller(true))
	responseFlavor.DefMethod(":trailer", "", respTrailerCaller(true))
	responseFlavor.DefMethod(":trailer-get", "", respTrailerGetCaller(true))
	responseFlavor.DefMethod(":body", "", respBodyCaller(true))
	responseFlavor.DefMethod(":close", "", respCloseCaller(true))

	return responseFlavor
}

type respStatusCaller bool

func (caller respStatusCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":status", len(args), "0")
	}
	return slip.Fixnum((obj.Any.(*http.Response)).StatusCode)
}

func (caller respStatusCaller) Docs() string {
	return `__:status__ => _fixnum_

Returns the status code of the response.
`
}

type respProtocolCaller bool

func (caller respProtocolCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":protocol", len(args), "0")
	}
	return slip.String((obj.Any.(*http.Response)).Proto)
}

func (caller respProtocolCaller) Docs() string {
	return `__:protocol__ => _string_

Returns the protocol of the response.
`
}

type respContentCaller bool

func (caller respContentCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":content", len(args), "0")
	}
	content, err := io.ReadAll(obj.Any.(*http.Response).Body)
	_ = obj.Any.(*http.Response).Body.Close()
	if err != nil {
		panic(err)
	}
	return slip.String(content)
}

func (caller respContentCaller) Docs() string {
	return `__:content__ => _string_

Returns the content of the response as a string.
`
}

type respContentLengthCaller bool

func (caller respContentLengthCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":content-length", len(args), "0")
	}
	return slip.Fixnum((obj.Any.(*http.Response)).ContentLength)
}

func (caller respContentLengthCaller) Docs() string {
	return `__:content-length__ => _fixnum_

Returns the content length of the response.
`
}

type respHeaderCaller bool

func (caller respHeaderCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
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

func (caller respHeaderCaller) Docs() string {
	return `__:header__ => _assoc-list_

Returns the header of the response as an association list.
`
}

type respHeaderGetCaller bool

func (caller respHeaderGetCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
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

func (caller respHeaderGetCaller) Docs() string {
	return `__:header-get__ _key_ => _string_|_nil_
   _key_ for the header value to get

Returns the header of the response as an association list.
`
}

type respTrailerCaller bool

func (caller respTrailerCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
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

func (caller respTrailerCaller) Docs() string {
	return `__:trailer__ => _assoc-list_

Returns the trailer of the response as an association list.
`
}

type respTrailerGetCaller bool

func (caller respTrailerGetCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
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

func (caller respTrailerGetCaller) Docs() string {
	return `__:trailer__ _key_ => _string_|_nil_
   _key_ for the trailer value to get

Returns the trailer of the response as an association list.
`
}

type respBodyCaller bool

func (caller respBodyCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":body", len(args), "0")
	}
	return &slip.InputStream{Reader: (obj.Any.(*http.Response)).Body}
}

func (caller respBodyCaller) Docs() string {
	return `__:body__ => _input-stream_

Returns the body, an input-stream of the response.
`
}

type respCloseCaller bool

func (caller respCloseCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":close", len(args), "0")
	}
	_ = (obj.Any.(*http.Response)).Body.Close()
	return nil
}

func (caller respCloseCaller) Docs() string {
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
