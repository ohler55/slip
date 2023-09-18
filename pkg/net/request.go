// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"io"
	"net/http"
	"net/url"
	"strconv"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	requestFlavor *flavors.Flavor
	methodChoices = map[string]bool{
		"GET":     true,
		"HEAD":    true,
		"POST":    true,
		"PUT":     true,
		"PATCH":   true,
		"DELETE":  true,
		"CONNECT": true,
		"OPTIONS": true,
		"TRACE":   true,
	}
)

func init() {
	requestFlavor = flavors.DefFlavor("http-request-flavor", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":init-keywords"),
				slip.Symbol(":method"),
				slip.Symbol(":protocol"),
				slip.Symbol(":url"),
				slip.Symbol(":remote-addr"),
				slip.Symbol(":header"),
				slip.Symbol(":trailer"),
				slip.Symbol(":content-length"),
				slip.Symbol(":body"),
			},
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A request is returned from an HTTP client request. It contains
the data associated with the HTTP reply.`),
			},
		},
	)
	requestFlavor.Final = true
	requestFlavor.DefMethod(":init", "", reqInitCaller(true))
	requestFlavor.DefMethod(":method", "", reqMethodCaller(true))
	requestFlavor.DefMethod(":protocol", "", reqProtocolCaller(true))
	requestFlavor.DefMethod(":url", "", reqURLCaller(true))
	requestFlavor.DefMethod(":remote-addr", "", reqRemoteAddrCaller(true))
	requestFlavor.DefMethod(":content-length", "", reqContentLengthCaller(true))
	requestFlavor.DefMethod(":header", "", reqHeaderCaller(true))
	requestFlavor.DefMethod(":header-get", "", reqHeaderGetCaller(true))
	requestFlavor.DefMethod(":trailer", "", reqTrailerCaller(true))
	requestFlavor.DefMethod(":trailer-get", "", reqTrailerGetCaller(true))
	requestFlavor.DefMethod(":body", "", reqBodyCaller(true))
	requestFlavor.DefMethod(":close", "", reqCloseCaller(true))
	requestFlavor.DefMethod(":write", "", reqWriteCaller(true))

	// TBD set methods for use with client?
}

type reqInitCaller bool

func getStrArg(arg slip.Object, use string) string {
	ss, ok := arg.(slip.String)
	if !ok {
		slip.PanicType(use, arg, "string")
	}
	return string(ss)
}

func assocToHeader(value slip.Object, field string) http.Header {
	alist, ok := value.(slip.List)
	if !ok {
		slip.PanicType(field, value, "assoc")
	}
	header := http.Header{}
	for _, element := range alist {
		elist, ok2 := element.(slip.List)
		if !ok2 || len(elist) < 2 {
			slip.PanicType("assoc element", element, "list")
		}
		var (
			key    slip.String
			values []string
		)
		if key, ok = elist[0].(slip.String); !ok {
			slip.PanicType("header key", elist[0], "string")
		}
		for _, v := range elist[1:] {
			var ss slip.String
			if ss, ok = v.(slip.String); !ok {
				slip.PanicType("header value", v, "string")
			}
			values = append(values, string(ss))
		}
		header[string(key)] = values
	}
	return header
}

func (caller reqInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	req := http.Request{}
	obj.Any = &req
	for i := 0; i < len(args); i += 2 {
		key, _ := args[i].(slip.Symbol)
		k := string(key)
		switch {
		case strings.EqualFold(":method", k):
			req.Method = getStrArg(args[i+1], k)
			if !methodChoices[req.Method] {
				slip.PanicType("method", slip.String(req.Method),
					"GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "CONNECT", "OPTIONS", "TRACE")
			}
		case strings.EqualFold(":protocol", k):
			req.Proto = getStrArg(args[i+1], k)
			if pos := strings.IndexByte(req.Proto, '/'); 0 < pos {
				pos++
				if dot := strings.IndexByte(req.Proto[pos:], '.'); 0 < dot {
					if num, err := strconv.ParseInt(req.Proto[pos:pos+dot], 10, 64); err == nil {
						req.ProtoMajor = int(num)
					} else {
						panic(err)
					}
					if num, err := strconv.ParseInt(req.Proto[pos+dot+1:], 10, 64); err == nil {
						req.ProtoMinor = int(num)
					} else {
						panic(err)
					}
				}
			}
		case strings.EqualFold(":url", k):
			var err error
			req.RequestURI = getStrArg(args[i+1], k)
			if req.URL, err = url.Parse(req.RequestURI); err != nil {
				panic(err)
			}
			req.Host = req.URL.Host
		case strings.EqualFold(":remote-addr", k):
			req.RemoteAddr = getStrArg(args[i+1], k)
		case strings.EqualFold(":header", k):
			req.Header = assocToHeader(args[i+1], ":header")
		case strings.EqualFold(":trailer", k):
			req.Trailer = assocToHeader(args[i+1], ":trailer")
		case strings.EqualFold(":content-length", k):
			num, ok := args[i+1].(slip.Integer)
			if !ok {
				slip.PanicType(":content-length", args[i+1], "integer")
			}
			req.ContentLength = num.Int64()
		case strings.EqualFold(":body", k):
			switch tb := args[i+1].(type) {
			case slip.String:
				req.Body = bodyWrapString(string(tb))
			case io.ReadCloser:
				req.Body = tb
			default:
				slip.PanicType(":body", args[i+1], "string", "input-stream")
			}
		}
	}
	return nil
}

func (caller reqInitCaller) Docs() string {
	return `__:init__ &key _method_ _protocol_ _url_ _remote-addr_ _header_ _trailer_ _content-length_ _body_
   _:method_ one of "GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "CONNECT", "OPTIONS", or "TRACE"
   _:protocol_ such as "HTTP/1.0"
   _:url_ for the request
   _:remote-addr_ remote address of the request
   _:header_ an association list with the values as a list such as (("Content-Length" 123))
   _:trailer_ an association list
   _:content-length_ is the length of the body
   _:body_ can be a _string_ or an _input-stream_.

Sets the initial values when _make-instance_ is called.
`
}

type reqMethodCaller bool

func (caller reqMethodCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":method", len(args), "0")
	}
	return slip.String((obj.Any.(*http.Request)).Method)
}

func (caller reqMethodCaller) Docs() string {
	return `__:method__ => _string_

Returns the method of the request.
`
}

type reqProtocolCaller bool

func (caller reqProtocolCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":protocol", len(args), "0")
	}
	return slip.String((obj.Any.(*http.Request)).Proto)
}

func (caller reqProtocolCaller) Docs() string {
	return `__:protocol__ => _string_

Returns the protocol of the request.
`
}

type reqURLCaller bool

func (caller reqURLCaller) Call(s *slip.Scope, args slip.List, _ int) (us slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":url", len(args), "0")
	}
	if obj.Any.(*http.Request).URL != nil {
		us = slip.String(obj.Any.(*http.Request).URL.String())
	}
	return
}

func (caller reqURLCaller) Docs() string {
	return `__:url__ => _string_

Returns the url of the request.
`
}

type reqRemoteAddrCaller bool

func (caller reqRemoteAddrCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":url", len(args), "0")
	}
	return slip.String(obj.Any.(*http.Request).RemoteAddr)
}

func (caller reqRemoteAddrCaller) Docs() string {
	return `__:remote-addr__ => _string_

Returns the remote address of the request.
`
}

type reqContentLengthCaller bool

func (caller reqContentLengthCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":content-length", len(args), "0")
	}
	return slip.Fixnum((obj.Any.(*http.Request)).ContentLength)
}

func (caller reqContentLengthCaller) Docs() string {
	return `__:contentLength__ => _fixnum_

Returns the contentLength of the request.
`
}

type reqHeaderCaller bool

func (caller reqHeaderCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":header", len(args), "0")
	}
	var header slip.List
	for k, v := range (obj.Any.(*http.Request)).Header {
		a := make(slip.List, len(v)+1)
		a[0] = slip.String(k)
		for i, vs := range v {
			a[i+1] = slip.String(vs)
		}
		header = append(header, a)
	}
	return header
}

func (caller reqHeaderCaller) Docs() string {
	return `__:header__ => _assoc-list_

Returns the header of the request as an association list.
`
}

type reqHeaderGetCaller bool

func (caller reqHeaderGetCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		flavors.PanicMethodArgChoice(obj, ":header-get", len(args), "1")
	}
	key, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("request :header-get key", args[0], "string")
	}
	header := (obj.Any.(*http.Request)).Header
	if value := header.Get(string(key)); 0 < len(value) {
		return slip.String(value)
	}
	return nil
}

func (caller reqHeaderGetCaller) Docs() string {
	return `__:header-get__ _key_ => _string_|_nil_
   _key_ for the header value to get

Returns the header of the request as an association list.
`
}

type reqTrailerCaller bool

func (caller reqTrailerCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":trailer", len(args), "0")
	}
	var trailer slip.List
	for k, v := range (obj.Any.(*http.Request)).Trailer {
		a := make(slip.List, len(v)+1)
		a[0] = slip.String(k)
		for i, vs := range v {
			a[i+1] = slip.String(vs)
		}
		trailer = append(trailer, a)
	}
	return trailer
}

func (caller reqTrailerCaller) Docs() string {
	return `__:trailer__ => _assoc-list_

Returns the trailer of the request as an association list.
`
}

type reqTrailerGetCaller bool

func (caller reqTrailerGetCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		flavors.PanicMethodArgChoice(obj, ":trailer-get", len(args), "1")
	}
	key, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("request :trailer-get key", args[0], "string")
	}
	trailer := (obj.Any.(*http.Request)).Trailer
	if value := trailer.Get(string(key)); 0 < len(value) {
		return slip.String(value)
	}
	return nil
}

func (caller reqTrailerGetCaller) Docs() string {
	return `__:trailer__ _key_ => _string_|_nil_
   _key_ for the trailer value to get

Returns the trailer of the request as an association list.
`
}

type reqBodyCaller bool

func (caller reqBodyCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":body", len(args), "0")
	}
	return &slip.InputStream{Reader: (obj.Any.(*http.Request)).Body}
}

func (caller reqBodyCaller) Docs() string {
	return `__:body__ => _input-stream_

Returns the body, an input-stream of the request.
`
}

type reqCloseCaller bool

func (caller reqCloseCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":close", len(args), "0")
	}
	_ = (obj.Any.(*http.Request)).Body.Close()
	return nil
}

func (caller reqCloseCaller) Docs() string {
	return `__:close__ => _nil_

Closes the body of the request.
`
}

type reqWriteCaller bool

func (caller reqWriteCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	var (
		dest slip.Object
	)
	if 0 < len(args) {
		dest = args[0]
		if 1 < len(args) {
			flavors.PanicMethodArgChoice(obj, ":write", len(args), "0 or 1")
		}
	}
	switch td := dest.(type) {
	case nil:
		var b strings.Builder
		_ = (obj.Any.(*http.Request)).Write(&b)
		return slip.String(b.String())
	case io.Writer:
		if err := (obj.Any.(*http.Request)).Write(td); err != nil {
			panic(err)
		}
	default:
		if dest == slip.True {
			w := s.Get(slip.Symbol("*standard-output*")).(io.Writer)
			if err := (obj.Any.(*http.Request)).Write(w); err != nil {
				panic(err)
			}
		} else {
			slip.PanicType("request :write destination", dest, "nil", "t", "output-stream")
		}
	}
	return nil
}

func (caller reqWriteCaller) Docs() string {
	return `__:write__ &optional _destination_ => _nil_
   _destination_ to write to


Writes the body of the request to the _destination_. If _destination_ is nil then
output is to a _string_ which is returned. If _destination_ is _t_ then output is
written to _*standard-output*_. If _destination_ is an _output-stream_ then
output is written to that _stream_.
`
}

// MakeRequest makes a new request.
func MakeRequest(req *http.Request) (inst *flavors.Instance) {
	inst = requestFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = req
	return
}
