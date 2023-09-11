// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"net/http"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

const defaultClientTimeout = time.Second * 10

var clientFlavor *flavors.Flavor

func init() {
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
	)
	clientFlavor.Final = true
	clientFlavor.DefMethod(":get", "", getCaller(true))
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

type getCaller bool

func (caller getCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":get", len(args), "0")
	}
	client, u := ensureClient(obj)
	resp, err := client.Get(u)
	if err != nil {
		slip.NewPanic("GET with URL %s failed. %s", u, err)
	}
	return MakeResponse(resp)
}

func (caller getCaller) Docs() string {
	return `__:get__ => _response_

Makes an HTTP GET requests and returns the response.
If an error occurs it is raised with a panic that can be recovered.
`
}
