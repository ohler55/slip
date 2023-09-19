// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"net/http"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	serverFlavor *flavors.Flavor
)

func init() {
	serverFlavor = flavors.DefFlavor("http-server-flavor", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":init-keywords"),
				slip.Symbol(":address"),
				slip.Symbol(":tls-config"),
				slip.Symbol(":read-timeout"),
				slip.Symbol(":write-timeout"),
				slip.Symbol(":idle-timeout"),
				slip.Symbol(":maximum-header-length"),
			},
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A server is returned from an HTTP client server. It contains
the data associated with the HTTP reply.`),
			},
		},
	)
	serverFlavor.Final = true
	serverFlavor.DefMethod(":init", "", serverInitCaller(true))
	serverFlavor.DefMethod(":start", "", serverStartCaller(true))
	serverFlavor.DefMethod(":close", "", serverCloseCaller(true))
	serverFlavor.DefMethod(":shutdown", "", serverShutdownCaller(true))
	serverFlavor.DefMethod(":add-handler", "", serverAddHandlerCaller(true))
	serverFlavor.DefMethod(":handlers", "", serverHandlersCaller(true))
}

type serverInitCaller bool

func (caller serverInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	server := http.Server{}
	// TBD add mux
	obj.Any = &server
	for i := 0; i < len(args); i += 2 {
		key, _ := args[i].(slip.Symbol)
		k := string(key)
		switch {
		case strings.EqualFold(":address", k):
			server.Addr = getStrArg(args[i+1], k)
		case strings.EqualFold(":tls-config", k):
			// TBD
		case strings.EqualFold(":close", k):
			// TBD
		case strings.EqualFold(":shutdown", k):
			// TBD
		case strings.EqualFold(":add-handler", k):
			// TBD
		case strings.EqualFold(":handlers", k):
			// TBD
		}
	}
	return nil
}

func (caller serverInitCaller) Docs() string {
	return `__:init__ &key _address_ _tls-config_ _read-timeout_ _write-timeout_ _idle-timeout_ _maximum-header-length_
   _address_
   _tls-config_
   _read-timeout_
   _write-timeout_
   _idle-timeout_
   _maximum-header-length_

Sets the initial values when _make-instance_ is called.
`
}

type serverStartCaller bool

func (caller serverStartCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":start", len(args), "0")
	}
	// TBD
	return nil
}

func (caller serverStartCaller) Docs() string {
	return `__:start__ => _nil_

Starts the server.
`
}

type serverCloseCaller bool

func (caller serverCloseCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":close", len(args), "0")
	}
	// TBD
	return nil
}

func (caller serverCloseCaller) Docs() string {
	return `__:close__ => _nil_

Closes the server with a hard, immediate shutdown.
`
}

type serverShutdownCaller bool

func (caller serverShutdownCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":shutdown", len(args), "0")
	}
	// TBD
	return nil
}

func (caller serverShutdownCaller) Docs() string {
	return `__:shutdown__ => _nil_

Gracefully shuts down the server.
`
}

type serverAddHandlerCaller bool

func (caller serverAddHandlerCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 2 {
		flavors.PanicMethodArgChoice(obj, ":add-handler", len(args), "2")
	}
	// TBD
	return nil
}

func (caller serverAddHandlerCaller) Docs() string {
	return `__:add-handler__ _path_ _handler_ => _nil_
   _path_
   _handler_

Adds a _handler_ function to handle requests on the _path_ server.
`
}

type serverHandlersCaller bool

func (caller serverHandlersCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":handlers", len(args), "0")
	}
	// TBD handles as an assoc with path/handler
	return nil
}

func (caller serverHandlersCaller) Docs() string {
	return `__:add-handler__ => _assoc_

Returns the handlers for the server.
`
}
