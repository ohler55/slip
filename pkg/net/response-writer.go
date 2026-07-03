// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"net/http"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var responseWriterFlavor *flavors.Flavor

func defResponseWriter() *flavors.Flavor {
	responseWriterFlavor = flavors.DefFlavor("http-response-writer-flavor", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A response-writer is an argument to the server handler functions.`),
			},
		},
		&Pkg,
	)
	responseWriterFlavor.Final = true
	responseWriterFlavor.GoMakeOnly = true
	responseWriterFlavor.DefMethod(":write-status", "", respWriterStatusCaller(true))
	responseWriterFlavor.DefMethod(":write", "", respWriterWriteCaller(true))
	responseWriterFlavor.DefMethod(":header", "", respWriterHeaderCaller(true))
	responseWriterFlavor.DefMethod(":header-get", "", respWriterHeaderGetCaller(true))
	responseWriterFlavor.DefMethod(":header-set", "", respWriterHeaderSetCaller(true))
	responseWriterFlavor.DefMethod(":header-add", "", respWriterHeaderAddCaller(true))

	return responseWriterFlavor
}

type respWriterStatusCaller bool

func (caller respWriterStatusCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		slip.MethodArgChoicePanic(s, depth, obj, ":write-status", len(args), "1")
	}
	code, ok := args[0].(slip.Fixnum)
	if !ok {
		slip.TypePanic(s, depth, "response-writer :write-status status", args[0], "fixnum")
	}
	(obj.Any.(http.ResponseWriter)).WriteHeader(int(code))
	return nil
}

func (caller respWriterStatusCaller) Docs() string {
	return `__:write-status__ _code_ => _nil_
   _code_ the status code for the HTTP response.

Writes the status code as part of the response.
`
}

type respWriterWriteCaller bool

func (caller respWriterWriteCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		slip.MethodArgChoicePanic(s, depth, obj, ":write", len(args), "1")
	}
	content, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "response-writer :write content", args[0], "string")
	}
	if _, err := (obj.Any.(http.ResponseWriter)).Write([]byte(content)); err != nil {
		panic(err)
	}
	return nil
}

func (caller respWriterWriteCaller) Docs() string {
	return `__:write__ _content_ => _nil_
   _content_ to write as the response.

Write _content_ as the response.
`
}

type respWriterHeaderCaller bool

func (caller respWriterHeaderCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, obj, ":header", len(args), "0")
	}
	var header slip.List
	for k, v := range (obj.Any.(http.ResponseWriter)).Header() {
		a := make(slip.List, len(v)+1)
		a[0] = slip.String(k)
		for i, vs := range v {
			a[i+1] = slip.String(vs)
		}
		header = append(header, a)
	}
	return header
}

func (caller respWriterHeaderCaller) Docs() string {
	return `__:header__ => _assoc-list_

Returns the header of the responseWriter as an association list.
`
}

type respWriterHeaderGetCaller bool

func (caller respWriterHeaderGetCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		slip.MethodArgChoicePanic(s, depth, obj, ":header-get", len(args), "1")
	}
	key, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "responseWriter :header-get key", args[0], "string")
	}
	header := (obj.Any.(http.ResponseWriter)).Header()
	if value := header.Get(string(key)); 0 < len(value) {
		return slip.String(value)
	}
	return nil
}

func (caller respWriterHeaderGetCaller) Docs() string {
	return `__:header-get__ _key_ => _string_|_nil_
   _key_ for the header value to get

Returns the header of the responseWriter as an association list.
`
}

type respWriterHeaderSetCaller bool

func (caller respWriterHeaderSetCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 2 {
		slip.MethodArgChoicePanic(s, depth, obj, ":header-set", len(args), "2")
	}
	key, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "responseWriter :header-set key", args[0], "string")
	}
	value, ok := args[1].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "responseWriter :header-set value", args[1], "string")
	}
	(obj.Any.(http.ResponseWriter)).Header().Set(string(key), string(value))
	return nil
}

func (caller respWriterHeaderSetCaller) Docs() string {
	return `__:header-set__ _key_ _value_ => _nil_
   _key_ for the header value to set
   _value_ to set for the header

Sets the responseWriter header value for _key_ to _value_.
`
}

type respWriterHeaderAddCaller bool

func (caller respWriterHeaderAddCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 2 {
		slip.MethodArgChoicePanic(s, depth, obj, ":header-add", len(args), "2")
	}
	key, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "responseWriter :header-add key", args[0], "string")
	}
	value, ok := args[1].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "responseWriter :header-add value", args[1], "string")
	}
	(obj.Any.(http.ResponseWriter)).Header().Add(string(key), string(value))
	return nil
}

func (caller respWriterHeaderAddCaller) Docs() string {
	return `__:header-add__ _key_ _value_ => _nil_
   _key_ for the header value to add
   _value_ to add to the header

Adds _value_ to the responseWriter header values for _key_.
`
}

// MakeResponseWriter makes a new response-writer.
func MakeResponseWriter(rw http.ResponseWriter) (inst *flavors.Instance) {
	inst = responseWriterFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = rw
	return
}
