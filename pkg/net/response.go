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
	responseFlavor.DefMethod(":status", "", respStatusCaller{})
	responseFlavor.DefMethod(":protocol", "", respProtocolCaller{})
	responseFlavor.DefMethod(":content", "", respContentCaller{})
	responseFlavor.DefMethod(":content-length", "", respContentLengthCaller{})
	responseFlavor.DefMethod(":header", "", respHeaderCaller{})
	responseFlavor.DefMethod(":header-get", "", respHeaderGetCaller{})
	responseFlavor.DefMethod(":trailer", "", respTrailerCaller{})
	responseFlavor.DefMethod(":trailer-get", "", respTrailerGetCaller{})
	responseFlavor.DefMethod(":body", "", respBodyCaller{})
	responseFlavor.DefMethod(":close", "", respCloseCaller{})

	return responseFlavor
}

type respStatusCaller struct{}

func (caller respStatusCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.PanicMethodArgChoice(obj, ":status", len(args), "0")
	}
	return slip.Fixnum((obj.Any.(*http.Response)).StatusCode)
}

func (caller respStatusCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":protocol",
		Text:   `Returns the status of the response.`,
		Return: "fixnum",
	}
}

type respProtocolCaller struct{}

func (caller respProtocolCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.PanicMethodArgChoice(obj, ":protocol", len(args), "0")
	}
	return slip.String((obj.Any.(*http.Response)).Proto)
}

func (caller respProtocolCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":protocol",
		Text:   `Returns the protocol of the response.`,
		Return: "string",
	}
}

type respContentCaller struct{}

func (caller respContentCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.PanicMethodArgChoice(obj, ":content", len(args), "0")
	}
	content, err := io.ReadAll(obj.Any.(*http.Response).Body)
	_ = obj.Any.(*http.Response).Body.Close()
	if err != nil {
		panic(err)
	}
	return slip.String(content)
}

func (caller respContentCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":content",
		Text:   `Returns the content of the response as a string.`,
		Return: "string",
	}
}

type respContentLengthCaller struct{}

func (caller respContentLengthCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.PanicMethodArgChoice(obj, ":content-length", len(args), "0")
	}
	return slip.Fixnum((obj.Any.(*http.Response)).ContentLength)
}

func (caller respContentLengthCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":content-length",
		Text:   `Returns the content length of the response.`,
		Return: "fixnum",
	}
}

type respHeaderCaller struct{}

func (caller respHeaderCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.PanicMethodArgChoice(obj, ":header", len(args), "0")
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

func (caller respHeaderCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":header",
		Text:   `Returns the header of the response as an association list.`,
		Return: "association-list",
	}
}

type respHeaderGetCaller struct{}

func (caller respHeaderGetCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		slip.PanicMethodArgChoice(obj, ":header-get", len(args), "1")
	}
	key, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "response :header-get key", args[0], "string")
	}
	header := (obj.Any.(*http.Response)).Header
	if value := header.Get(string(key)); 0 < len(value) {
		return slip.String(value)
	}
	return nil
}

func (caller respHeaderGetCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":header-get",
		Text: `Returns the values for the provided key in the the response header.`,
		Args: []*slip.DocArg{
			{
				Name: "key",
				Type: "string|symbol",
				Text: `Key for the values to get from the response header.`,
			},
		},
		Return: "list",
	}
}

type respTrailerCaller struct{}

func (caller respTrailerCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.PanicMethodArgChoice(obj, ":trailer", len(args), "0")
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

func (caller respTrailerCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":trailer",
		Text:   `Returns the trailer of the response as an association list.`,
		Return: "association-list",
	}
}

type respTrailerGetCaller struct{}

func (caller respTrailerGetCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		slip.PanicMethodArgChoice(obj, ":trailer-get", len(args), "1")
	}
	key, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "response :trailer-get key", args[0], "string")
	}
	trailer := (obj.Any.(*http.Response)).Trailer
	if value := trailer.Get(string(key)); 0 < len(value) {
		return slip.String(value)
	}
	return nil
}

func (caller respTrailerGetCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":trailer-get",
		Text: `Returns the values for the provided key in the the response trailer.`,
		Args: []*slip.DocArg{
			{
				Name: "key",
				Type: "string|symbol",
				Text: `Key for the values to get from the response trailer.`,
			},
		},
		Return: "list",
	}
}

type respBodyCaller struct{}

func (caller respBodyCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.PanicMethodArgChoice(obj, ":body", len(args), "0")
	}
	return &slip.InputStream{Reader: (obj.Any.(*http.Response)).Body}
}

func (caller respBodyCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":body",
		Text:   `Returns the body of the response as an input-stream .`,
		Return: "input-stream",
	}
}

type respCloseCaller struct{}

func (caller respCloseCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		slip.PanicMethodArgChoice(obj, ":close", len(args), "0")
	}
	_ = (obj.Any.(*http.Response)).Body.Close()
	return nil
}

func (caller respCloseCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":close",
		Text: `Closes the body of the response .`,
	}
}

// MakeResponse makes a new response.
func MakeResponse(resp *http.Response) (inst *flavors.Instance) {
	inst = responseFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = resp
	return
}
