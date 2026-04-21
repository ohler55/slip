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

func defRequest() *flavors.Flavor {
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
		&Pkg,
	)
	requestFlavor.Final = true
	requestFlavor.DefMethod(":init", "", reqInitCaller{})
	requestFlavor.DefMethod(":method", "", reqMethodCaller{})
	requestFlavor.DefMethod(":protocol", "", reqProtocolCaller{})
	requestFlavor.DefMethod(":url", "", reqURLCaller{})
	requestFlavor.DefMethod(":remote-addr", "", reqRemoteAddrCaller{})
	requestFlavor.DefMethod(":content-length", "", reqContentLengthCaller{})
	requestFlavor.DefMethod(":header", "", reqHeaderCaller{})
	requestFlavor.DefMethod(":header-get", "", reqHeaderGetCaller{})
	requestFlavor.DefMethod(":trailer", "", reqTrailerCaller{})
	requestFlavor.DefMethod(":trailer-get", "", reqTrailerGetCaller{})
	requestFlavor.DefMethod(":body", "", reqBodyCaller{})
	requestFlavor.DefMethod(":close", "", reqCloseCaller(true))
	requestFlavor.DefMethod(":write", "", reqWriteCaller{})

	return requestFlavor
}

type reqInitCaller struct{}

func (caller reqInitCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
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
			req.Method = getStrArg(s, args[i+1], k, depth)
			if !methodChoices[req.Method] {
				slip.TypePanic(s, depth, "method", slip.String(req.Method),
					"GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "CONNECT", "OPTIONS", "TRACE")
			}
		case strings.EqualFold(":protocol", k):
			req.Proto = getStrArg(s, args[i+1], k, depth)
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
			req.RequestURI = getStrArg(s, args[i+1], k, depth)
			if req.URL, err = url.Parse(req.RequestURI); err != nil {
				panic(err)
			}
			req.Host = req.URL.Host
		case strings.EqualFold(":remote-addr", k):
			req.RemoteAddr = getStrArg(s, args[i+1], k, depth)
		case strings.EqualFold(":header", k):
			req.Header = assocToHeader(s, args[i+1], ":header", depth)
		case strings.EqualFold(":trailer", k):
			req.Trailer = assocToHeader(s, args[i+1], ":trailer", depth)
		case strings.EqualFold(":content-length", k):
			num, ok := args[i+1].(slip.Integer)
			if !ok {
				slip.TypePanic(s, depth, ":content-length", args[i+1], "integer")
			}
			req.ContentLength = num.Int64()
		case strings.EqualFold(":body", k):
			switch tb := args[i+1].(type) {
			case slip.String:
				req.Body = bodyWrapString(string(tb))
			case io.ReadCloser:
				req.Body = tb
			default:
				slip.TypePanic(s, depth, ":body", args[i+1], "string", "input-stream")
			}
		}
	}
	return nil
}

func (caller reqInitCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":init",
		Text: "Sets the initial value when _make-instance_ is called.",
		Args: []*slip.DocArg{
			{Name: "&key"},
			{
				Name: ":method",
				Type: "string",
				Text: `One of "GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "CONNECT", "OPTIONS", or "TRACE`,
			},
			{
				Name: ":url",
				Type: "string",
				Text: "URL for the request",
			},
			{
				Name: ":protocol",
				Type: "string",
				Text: `Such as "HTTP/1.0"`,
			},
			{
				Name: ":remote-addr",
				Type: "string",
				Text: "Remote address for the request",
			},
			{
				Name: ":header",
				Type: `An association list with the values as a list such as (("Content-Type" "application/json"))`,
				Text: "Headers for the request",
			},
			{
				Name: ":trailer",
				Type: `An association list with the values as a list such as (("Content-Type" "application/json"))`,
				Text: "Trailer for the request",
			},
			{
				Name: ":content-length",
				Type: "fixnum",
				Text: `The length of the content.`,
			},
			{
				Name: ":body",
				Type: "string|input-stream",
				Text: `Content for the body of the request`,
			},
		},
	}
}

type reqMethodCaller struct{}

func (caller reqMethodCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, obj, ":method", len(args), "0")
	}
	return slip.String((obj.Any.(*http.Request)).Method)
}

func (caller reqMethodCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":method",
		Text:   `Returns the method of the request.`,
		Return: "string",
	}
}

type reqProtocolCaller struct{}

func (caller reqProtocolCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, obj, ":protocol", len(args), "0")
	}
	return slip.String((obj.Any.(*http.Request)).Proto)
}

func (caller reqProtocolCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":protocol",
		Text:   `Returns the protocol of the request.`,
		Return: "string",
	}
}

type reqURLCaller struct{}

func (caller reqURLCaller) Call(s *slip.Scope, args slip.List, depth int) (us slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, obj, ":url", len(args), "0")
	}
	if obj.Any.(*http.Request).URL != nil {
		us = slip.String(obj.Any.(*http.Request).URL.String())
	}
	return
}

func (caller reqURLCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":url",
		Text:   `Returns the URL of the request.`,
		Return: "string",
	}
}

type reqRemoteAddrCaller struct{}

func (caller reqRemoteAddrCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, obj, ":url", len(args), "0")
	}
	return slip.String(obj.Any.(*http.Request).RemoteAddr)
}

func (caller reqRemoteAddrCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":remote-addr",
		Text:   `Returns the remote address of the request.`,
		Return: "string",
	}
}

type reqContentLengthCaller struct{}

func (caller reqContentLengthCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, obj, ":content-length", len(args), "0")
	}
	return slip.Fixnum((obj.Any.(*http.Request)).ContentLength)
}

func (caller reqContentLengthCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":content-length",
		Text:   `Returns the content length of the request.`,
		Return: "fixnum",
	}
}

type reqHeaderCaller struct{}

func (caller reqHeaderCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, obj, ":header", len(args), "0")
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

func (caller reqHeaderCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":header",
		Text:   `Returns the header of the request as an association list.`,
		Return: "association-list",
	}
}

type reqHeaderGetCaller struct{}

func (caller reqHeaderGetCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		slip.MethodArgChoicePanic(s, depth, obj, ":header-get", len(args), "1")
	}
	key, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "request :header-get key", args[0], "string")
	}
	header := (obj.Any.(*http.Request)).Header
	if value := header.Get(string(key)); 0 < len(value) {
		return slip.String(value)
	}
	return nil
}

func (caller reqHeaderGetCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":header-get",
		Text: `Returns the values for the provided key in the the request header.`,
		Args: []*slip.DocArg{
			{
				Name: "key",
				Type: "string|symbol",
				Text: `Key for the values to get from the request header.`,
			},
		},
		Return: "list",
	}
}

type reqTrailerCaller struct{}

func (caller reqTrailerCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, obj, ":trailer", len(args), "0")
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

func (caller reqTrailerCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":trailer",
		Text:   `Returns the trailer of the request as an association list.`,
		Return: "association-list",
	}
}

type reqTrailerGetCaller struct{}

func (caller reqTrailerGetCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		slip.MethodArgChoicePanic(s, depth, obj, ":trailer-get", len(args), "1")
	}
	key, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "request :trailer-get key", args[0], "string")
	}
	trailer := (obj.Any.(*http.Request)).Trailer
	if value := trailer.Get(string(key)); 0 < len(value) {
		return slip.String(value)
	}
	return nil
}

func (caller reqTrailerGetCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":trailer-get",
		Text: `Returns the values for the provided key in the the request trailer.`,
		Args: []*slip.DocArg{
			{
				Name: "key",
				Type: "string|symbol",
				Text: `Key for the values to get from the request trailer.`,
			},
		},
		Return: "list",
	}
}

type reqBodyCaller struct{}

func (caller reqBodyCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, obj, ":body", len(args), "0")
	}
	return slip.NewInputStream((obj.Any.(*http.Request)).Body)
}

func (caller reqBodyCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":body",
		Text: `Returns the body input-stream of the request.`,
	}
}

type reqCloseCaller bool

func (caller reqCloseCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.MethodArgChoicePanic(s, depth, obj, ":close", len(args), "0")
	}
	_ = (obj.Any.(*http.Request)).Body.Close()
	return nil
}

func (caller reqCloseCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":close",
		Text: `Closes the body of the request.`,
	}
}

type reqWriteCaller struct{}

func (caller reqWriteCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	var (
		dest slip.Object
	)
	if 0 < len(args) {
		dest = args[0]
		if 1 < len(args) {
			slip.MethodArgChoicePanic(s, depth, obj, ":write", len(args), "0 or 1")
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
			slip.TypePanic(s, depth, "request :write destination", dest, "nil", "t", "output-stream")
		}
	}
	return nil
}

func (caller reqWriteCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":write",
		Text: `Writes the body of the request to the _destination_. If _destination_ is nil then
output is to a _string_ which is returned. If _destination_ is _t_ then output is
written to _*standard-output*_. If _destination_ is an _output-stream_ then
output is written to that _stream_.`,
		Args: []*slip.DocArg{
			{Name: "&optional"},
			{
				Name: "destination",
				Type: "t|nil|output-stream",
				Text: `Destination for the write.`,
			},
		},
		Return: "nil|string",
	}
}

// MakeRequest makes a new request.
func MakeRequest(req *http.Request) (inst *flavors.Instance) {
	inst = requestFlavor.MakeInstance().(*flavors.Instance)
	inst.SetSynchronized(true)
	inst.Any = req
	return
}
