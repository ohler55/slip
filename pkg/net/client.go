// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"time"

	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/flavors"
)

const defaultClientTimeout = time.Second * 10

var clientFlavor *flavors.Flavor

func defClient() *flavors.Flavor {
	clientFlavor = flavors.DefFlavor("http-client-flavor",
		map[string]slip.Object{
			"timeout": slip.Fixnum(defaultClientTimeout),
			"url":     nil,
		},
		nil,
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A client is used for making HTTP requests.`),
			},
			slip.Symbol(":inittable-instance-variables"),
			slip.Symbol(":gettable-instance-variables"),
			slip.Symbol(":settable-instance-variables"),
		},
		&Pkg,
	)
	clientFlavor.Final = true
	clientFlavor.DefMethod(":connect", "", clientBodylessCaller(http.MethodConnect))
	clientFlavor.DefMethod(":delete", "", clientBodylessCaller(http.MethodDelete))
	clientFlavor.DefMethod(":get", "", clientBodylessCaller(http.MethodGet))
	clientFlavor.DefMethod(":head", "", clientBodylessCaller(http.MethodHead))
	clientFlavor.DefMethod(":options", "", clientBodylessCaller(http.MethodOptions))
	clientFlavor.DefMethod(":patch", "", clientBodyCaller(http.MethodPatch))
	clientFlavor.DefMethod(":post", "", clientBodyCaller(http.MethodPost))
	clientFlavor.DefMethod(":put", "", clientBodyCaller(http.MethodPut))
	clientFlavor.DefMethod(":trace", "", clientBodylessCaller(http.MethodTrace))

	return clientFlavor
}

func ensureClient(obj *flavors.Instance) (client *http.Client, u string) {
	timeout := defaultClientTimeout
	if num, ok := obj.Get("timeout").(slip.Fixnum); ok && 0 < num {
		timeout = time.Duration(num)
	}
	if obj.Any == nil {
		client = &http.Client{Timeout: timeout}
		obj.Any = client
	} else {
		client = obj.Any.(*http.Client)
		client.Timeout = timeout
	}
	if ss, ok := obj.Get(slip.Symbol("url")).(slip.String); !ok || len(ss) == 0 {
		slip.NewPanic("url is not set or is not a string")
	} else {
		u = string(ss)
	}
	return
}

func headerFromAssoc(s *slip.Scope, obj slip.Object, depth int) http.Header {
	alist, ok := obj.(slip.List)
	if !ok {
		slip.TypePanic(s, depth, "header", obj, "list")
	}
	header := http.Header{}
	for _, entry := range alist {
		var (
			elist  slip.List
			ss     slip.String
			name   string
			values []string
		)
		if elist, ok = entry.(slip.List); !ok || len(elist) < 1 {
			slip.TypePanic(s, depth, "header field", obj, "list")
		}
		if ss, ok = elist[0].(slip.String); ok {
			name = string(ss)
		} else {
			slip.TypePanic(s, depth, "header field name", elist[0], "string")
		}
		for _, sv := range elist[1:] {
			if ss, ok = sv.(slip.String); ok {
				values = append(values, string(ss))
			} else {
				slip.TypePanic(s, depth, "header field value", sv, "string")
			}
		}
		header[name] = values
	}
	return header
}

type clientBodylessCaller string

func (caller clientBodylessCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	header := http.Header{}
	switch len(args) {
	case 0:
		// no header
	case 1:
		header = headerFromAssoc(s, args[0], depth)
	default:
		slip.MethodArgChoicePanic(s, depth, obj, ":"+string(caller), len(args), "0 or 1")
	}
	client, u := ensureClient(obj)
	req := http.Request{
		Method: string(caller),
		Header: header,
	}
	var (
		resp *http.Response
		err  error
	)
	if req.URL, err = url.Parse(u); err != nil {
		panic(err)
	}
	if resp, err = client.Do(&req); err != nil {
		slip.NewPanic("%s with URL %s failed. %s", caller, u, err)
	}
	return MakeResponse(resp)
}

func (caller clientBodylessCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":" + strings.ToLower(string(caller)),
		Text: fmt.Sprintf(`Makes an HTTP %s requests and returns the response.
If an error occurs it is raised with a panic.`, caller),
		Args: []*slip.DocArg{
			{Name: "&optional"},
			{
				Name: "headers",
				Type: "association-list",
				Text: `An association list with list values such as (("Content-Type" "application/json")).`,
			},
		},
		Return: "http-response",
	}
}

type clientBodyCaller string

func wrapContent(s *slip.Scope, arg slip.Object, depth int) (rc io.ReadCloser, ct string) {
	switch ta := arg.(type) {
	case slip.String:
		rc = bodyWrapString(string(ta))
	case *flavors.Instance:
		if ta.Type != bag.Flavor() {
			slip.TypePanic(s, depth, "content", arg, "string", "bag", "input-stream")
		}
		rc = bodyWrapString(oj.JSON(ta.Any))
		ct = "application/json"
	case io.Reader:
		rc = bodyWrapReader(ta)
	default:
		slip.TypePanic(s, depth, "content", arg, "string", "bag", "input-stream")
	}
	return
}

func (caller clientBodyCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	header := http.Header{}
	var (
		resp *http.Response
		err  error
		ct   string
		body io.ReadCloser
	)
	switch len(args) {
	case 1:
		body, ct = wrapContent(s, args[0], depth)
		if 0 < len(ct) {
			header.Set("Content-Type", ct)
		}
	case 2:
		body, ct = wrapContent(s, args[0], depth)
		if 0 < len(ct) {
			header.Set("Content-Type", ct)
		}
		switch ta := args[1].(type) {
		case nil:
			// no change
		case slip.String:
			ct = string(ta)
			header.Set("Content-Type", ct)
		default:
			slip.TypePanic(s, depth, "content-type", ta, "string")
		}
	case 3:
		body, ct = wrapContent(s, args[0], depth)
		if 0 < len(ct) {
			header.Set("Content-Type", ct)
		}
		switch ta := args[1].(type) {
		case nil:
			// no change
		case slip.String:
			header.Set("Content-Type", string(ta))
		default:
			slip.TypePanic(s, depth, "content-type", ta, "string")
		}
		header = headerFromAssoc(s, args[2], depth)
	default:
		slip.MethodArgChoicePanic(s, depth, obj, ":"+string(caller), len(args), "1, 2, or 3")
	}
	client, u := ensureClient(obj)
	req := http.Request{
		Method: string(caller),
		Header: header,
		Body:   body,
	}
	if req.URL, err = url.Parse(u); err != nil {
		panic(err)
	}
	if resp, err = client.Do(&req); err != nil {
		slip.NewPanic("%s with URL %s failed. %s", caller, u, err)
	}
	return MakeResponse(resp)
}

func (caller clientBodyCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":" + strings.ToLower(string(caller)),
		Text: fmt.Sprintf(`Makes an HTTP %s requests and returns the response. If the _content_ is a
string then it is used as the body of the request. If the _content_ is an instance of the _bag-flavor_
then the _content_ is encoded as JSON and used as the body of the request. If the _content-type_ is
_nil_ then it will be set to "application/json" when the _content_ is a _bag_. If the _content_ is an
_input-stream_ then the stream will be read to form the body of the request.


If an error occurs it is raised with a panic.`, caller),
		Args: []*slip.DocArg{
			{
				Name: "content",
				Type: "string|bag|input-stream",
				Text: `The content for the body of the request.`,
			},
			{Name: "&optional"},
			{
				Name: "headers",
				Type: "association-list",
				Text: `An association list with list values such as (("Content-Type" "application/json")).`,
			},
			{
				Name: "content-type",
				Type: "string",
				Text: `the content type such as "application/json".`,
			},
		},
		Return: "http-response",
	}
}
