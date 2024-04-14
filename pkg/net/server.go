// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"context"
	"fmt"
	"net/http"
	"path/filepath"
	"strings"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	serverFlavor *flavors.Flavor
)

func defServer() {
	Pkg.Initialize(nil)
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
				slip.String(`An HTTP server is returned.`),
			},
		},
		&Pkg,
	)
	serverFlavor.Final = true
	serverFlavor.DefMethod(":init", "", serverInitCaller(true))
	serverFlavor.DefMethod(":start", "", serverStartCaller(true))
	serverFlavor.DefMethod(":shutdown", "", serverShutdownCaller(true))
	serverFlavor.DefMethod(":add-handler", "", serverAddHandlerCaller(true))
}

type serverInitCaller bool

func (caller serverInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	server := http.Server{Handler: http.NewServeMux()}
	obj.Any = &server
	for i := 0; i < len(args); i += 2 {
		key, _ := args[i].(slip.Symbol)
		k := string(key)
		switch {
		case strings.EqualFold(":address", k):
			server.Addr = getStrArg(args[i+1], k)
		case strings.EqualFold(":tls-config", k):
			// TBD
		case strings.EqualFold(":read-timeout", k):
			server.ReadTimeout = getDurationArg(args[i+1], k)
		case strings.EqualFold(":write-timeout", k):
			server.WriteTimeout = getDurationArg(args[i+1], k)
		case strings.EqualFold(":idle-timeout", k):
			server.IdleTimeout = getDurationArg(args[i+1], k)
		case strings.EqualFold(":maximum-header-length", k):
			server.MaxHeaderBytes = int(getIntArg(args[i+1], k))
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
	if 1 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":start", len(args), "0 or 1")
	}
	// TBD if server.TLSConfig is not nil then ListenAndServeTLS()

	server := obj.Any.(*http.Server)
	go func() { _ = server.ListenAndServe() }()
	if 0 < len(args) {
		// TBD create URL from server.Addr
		var u string
		if 0 < len(server.Addr) && server.Addr[0] == ':' {
			u = fmt.Sprintf("http://localhost%s", server.Addr)
		} else {
			u = fmt.Sprintf("http://%s", server.Addr)
		}
		dur := getDurationArg(args[0], "wait-ready")
		start := time.Now()
		var err error
		for time.Since(start) < dur {
			time.Sleep(time.Millisecond * 10)
			if _, err = http.Get(u); err == nil {
				break
			}
		}
		if err != nil {
			panic(err)
		}
	}
	return nil
}

func (caller serverStartCaller) Docs() string {
	return `__:start__ &optional _wait-ready_ => _nil_
   _wait-ready_ is the maximum duration in seconds to wait for the server to be ready.

Starts the server.
`
}

type serverShutdownCaller bool

func (caller serverShutdownCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 1 < len(args) {
		flavors.PanicMethodArgChoice(obj, ":shutdown", len(args), "0 or 1")
	}
	server := obj.Any.(*http.Server)
	if 0 < len(args) && args[0] != nil {
		_ = server.Close()
	} else {
		cx, cf := context.WithTimeout(context.Background(), server.IdleTimeout)
		defer cf()
		_ = server.Shutdown(cx)
	}
	return nil
}

func (caller serverShutdownCaller) Docs() string {
	return `__:shutdown__ &optional _immediate_ => _nil_
   _immediate_ if true shuts down immediately and not gracefully.

Shuts down the server.
`
}

type serverAddHandlerCaller bool

func (caller serverAddHandlerCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 2 {
		flavors.PanicMethodArgChoice(obj, ":add-handler", len(args), "2")
	}
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("add-handler path", args[0], "string")
	}
	server := obj.Any.(*http.Server)
	if fpath, ok := args[1].(slip.String); ok {
		server.Handler.(*http.ServeMux).HandleFunc(
			string(path),
			func(w http.ResponseWriter, r *http.Request) {
				p := filepath.Join(string(fpath), strings.TrimPrefix(r.URL.Path, string(path)))
				http.ServeFile(w, r, p)
			},
		)
		return nil
	}
	reqCaller := cl.ResolveToCaller(s, args[1], depth)
	server.Handler.(*http.ServeMux).HandleFunc(
		string(path),
		func(w http.ResponseWriter, r *http.Request) {
			cargs := slip.List{
				MakeResponseWriter(w),
				MakeRequest(r),
			}
			reqCaller.Call(s, cargs, depth+1)
		},
	)
	return nil
}

func (caller serverAddHandlerCaller) Docs() string {
	return `__:add-handler__ _path_ _handler_ => _nil_
   _path_ to be handled by the _handler_.
   _handler_ function to handle requests on _path_ or a _string_ that is the path to a directory.

Adds a _handler_ function to handle requests on the _path_ server.
`
}
