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

var serverFlavor *flavors.Flavor

func defServer() *flavors.Flavor {
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
	serverFlavor.DefMethod(":init", "", serverInitCaller{})
	serverFlavor.DefMethod(":start", "", serverStartCaller{})
	serverFlavor.DefMethod(":shutdown", "", serverShutdownCaller{})
	serverFlavor.DefMethod(":add-handler", "", serverAddHandlerCaller{})

	return serverFlavor
}

type serverInitCaller struct{}

func (caller serverInitCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
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
			server.Addr = getStrArg(s, args[i+1], k, depth)
		case strings.EqualFold(":tls-config", k):
			// TBD
		case strings.EqualFold(":read-timeout", k):
			server.ReadTimeout = getDurationArg(s, args[i+1], k, depth)
		case strings.EqualFold(":write-timeout", k):
			server.WriteTimeout = getDurationArg(s, args[i+1], k, depth)
		case strings.EqualFold(":idle-timeout", k):
			server.IdleTimeout = getDurationArg(s, args[i+1], k, depth)
		case strings.EqualFold(":maximum-header-length", k):
			server.MaxHeaderBytes = int(getIntArg(s, args[i+1], k, depth))
		}
	}
	return nil
}

func (caller serverInitCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":init",
		Text: "Sets the initial value when _make-instance_ is called.",
		Args: []*slip.DocArg{
			{Name: "&key"},
			{
				Name: ":address",
				Type: "string",
				Text: `The server address and port number (e.g, :8080 or 127.0.0.1:8080)`,
			},
			{
				Name: ":read-timeout",
				Type: "real",
				Text: `The read timeout in seconds.`,
			},
			{
				Name: ":write-timeout",
				Type: "real",
				Text: `The write timeout in seconds.`,
			},
			{
				Name: ":idle-timeout",
				Type: "real",
				Text: `The idle timeout in seconds.`,
			},
			{
				Name: ":maximum-header-length",
				Type: "fixnum",
				Text: `The maximum header length.`,
			},
			{
				Name: ":tls-config",
				Type: "string",
				Text: "Not implemented yet. Ignored.",
			},
		},
	}
}

type serverStartCaller struct{}

func (caller serverStartCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 1 < len(args) {
		slip.MethodArgChoicePanic(s, depth, obj, ":start", len(args), "0 or 1")
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
		dur := getDurationArg(s, args[0], "wait-ready", depth)
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

func (caller serverStartCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":start",
		Text: `Starts the server.`,
		Args: []*slip.DocArg{
			{Name: "&optional"},
			{
				Name: "wait-ready",
				Type: "fixnum",
				Text: `The maximum duration in seconds to wait for the server to be ready.`,
			},
		},
	}
}

type serverShutdownCaller struct{}

func (caller serverShutdownCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 1 < len(args) {
		slip.MethodArgChoicePanic(s, depth, obj, ":shutdown", len(args), "0 or 1")
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

func (caller serverShutdownCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":shutdown",
		Text: `Shuts down the server.`,
		Args: []*slip.DocArg{
			{Name: "&optional"},
			{
				Name: "immediate",
				Type: "boolean",
				Text: `If true shuts down immediately and not gracefully.`,
			},
		},
	}
}

type serverAddHandlerCaller struct{}

func (caller serverAddHandlerCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 2 {
		slip.MethodArgChoicePanic(s, depth, obj, ":add-handler", len(args), "2")
	}
	path, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "add-handler path", args[0], "string")
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

func (caller serverAddHandlerCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":add-handler",
		Text: `Adds a _handler_ function to handle requests on the _path_ server.`,
		Args: []*slip.DocArg{
			{
				Name: "path",
				Type: "string",
				Text: `Path to be handled by _handler_.`,
			},
			{
				Name: "handler",
				Type: "symbol|function|string",
				Text: `Function to handle requests on _path_ or a _string_ that is the path to a directory.`,
			},
		},
	}
}
