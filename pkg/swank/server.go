// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"fmt"
	"net"
	"sync"
	"sync/atomic"

	"github.com/ohler55/slip"
)

// Server manages Swank connections for SLIME integration.
type Server struct {
	listener    net.Listener
	connections sync.Map // map[int64]*Connection
	nextConnID  atomic.Int64
	baseScope   *slip.Scope
	running     atomic.Bool
	address     string
}

// NewServer creates a new Swank server with the given base scope.
// Each connection will get a child scope of this base scope.
func NewServer(scope *slip.Scope) *Server {
	return &Server{
		baseScope: scope,
	}
}

// Start begins listening for connections on the specified address.
// The address should be in the form "host:port" or ":port".
func (s *Server) Start(address string) error {
	listener, err := net.Listen("tcp", address)
	if err != nil {
		return err
	}
	s.listener = listener
	s.address = listener.Addr().String()
	s.running.Store(true)

	go s.acceptLoop()
	return nil
}

// Stop shuts down the server and closes all connections.
func (s *Server) Stop() error {
	if !s.running.CompareAndSwap(true, false) {
		return nil // already stopped
	}

	// Close listener to stop accept loop
	if s.listener != nil {
		s.listener.Close()
	}

	// Close all connections
	s.connections.Range(func(key, value any) bool {
		if conn, ok := value.(*Connection); ok {
			conn.Close()
		}
		return true
	})

	return nil
}

// Addr returns the address the server is listening on.
func (s *Server) Addr() string {
	return s.address
}

// Running returns true if the server is accepting connections.
func (s *Server) Running() bool {
	return s.running.Load()
}

// acceptLoop handles incoming connections.
func (s *Server) acceptLoop() {
	for s.running.Load() {
		conn, err := s.listener.Accept()
		if err != nil {
			if s.running.Load() {
				// Log error but continue accepting
				LogError("accept error: %v", err)
			}
			continue
		}

		id := s.nextConnID.Add(1)
		swankConn := NewConnection(id, conn, s)
		s.connections.Store(id, swankConn)

		go swankConn.Run()
	}
}

// removeConnection removes a connection from the server's tracking.
func (s *Server) removeConnection(id int64) {
	s.connections.Delete(id)
}

// defSwankServer defines the swank-server function.
func defSwankServer() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SwankServer{Function: slip.Function{Name: "swank-server", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "swank-server",
			Args: []*slip.DocArg{
				{
					Name: "&key",
				},
				{
					Name: "port",
					Type: "fixnum",
					Text: "port to listen on (default 4005).",
				},
				{
					Name: "host",
					Type: "string",
					Text: "host/interface to bind to (default all interfaces).",
				},
			},
			Return: "swank-server",
			Text: `__swank-server__ starts a Swank server for SLIME integration.
The server listens for connections from Emacs SLIME and handles RPC requests
for evaluation, completion, and other IDE features.

Once started, connect from Emacs with:
  M-x slime-connect RET localhost RET 4005 RET`,
			Examples: []string{
				`(swank-server) => #<swank-server :4005>`,
				`(swank-server :port 5005) => #<swank-server :5005>`,
			},
		}, &Pkg)
}

// SwankServer represents the swank-server function.
type SwankServer struct {
	slip.Function
}

// Call starts the Swank server.
func (f *SwankServer) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 4)

	port := slip.Fixnum(4005)
	host := ""

	for i := 0; i < len(args); i += 2 {
		key, ok := args[i].(slip.Symbol)
		if !ok || i+1 >= len(args) {
			slip.TypePanic(s, depth, "keyword", args[i], ":port", ":host")
		}
		switch key {
		case ":port":
			if p, ok := args[i+1].(slip.Fixnum); ok {
				port = p
			} else {
				slip.TypePanic(s, depth, ":port", args[i+1], "fixnum")
			}
		case ":host":
			if h, ok := args[i+1].(slip.String); ok {
				host = string(h)
			} else {
				slip.TypePanic(s, depth, ":host", args[i+1], "string")
			}
		default:
			slip.TypePanic(s, depth, "keyword", args[i], ":port", ":host")
		}
	}

	return startDefaultServer(s, depth, port, host)
}

// SwankServerInstance represents a running Swank server.
type SwankServerInstance struct {
	server *Server
}

// Addr returns the address the underlying server is listening on.
func (s *SwankServerInstance) Addr() string {
	return s.server.Addr()
}

func (s *SwankServerInstance) String() string {
	return fmt.Sprintf("#<swank-server %s>", s.server.Addr())
}

func (s *SwankServerInstance) Append(b []byte) []byte {
	return append(b, s.String()...)
}

func (s *SwankServerInstance) Simplify() any {
	return s.String()
}

func (s *SwankServerInstance) Equal(other slip.Object) bool {
	if o, ok := other.(*SwankServerInstance); ok {
		return s.server == o.server
	}
	return false
}

func (s *SwankServerInstance) Hierarchy() []slip.Symbol {
	return []slip.Symbol{"swank-server", slip.TrueSymbol}
}

func (s *SwankServerInstance) Eval(scope *slip.Scope, depth int) slip.Object {
	return s
}

// defSwankStop defines the swank-stop function.
func defSwankStop() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SwankStop{Function: slip.Function{Name: "swank-stop", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "swank-stop",
			Args:   []*slip.DocArg{},
			Return: "nil",
			Text:   `__swank-stop__ stops the default Swank server.`,
			Examples: []string{
				`(swank-stop) => nil`,
			},
		}, &Pkg)
}

// SwankStop represents the swank-stop function.
type SwankStop struct {
	slip.Function
}

// Call stops the default Swank server.
func (f *SwankStop) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 0)

	if defaultServer != nil {
		_ = defaultServer.Stop()
		defaultServer = nil
	}
	return nil
}

// startDefaultServer is the shared implementation for create-server,
// start-server, setup-server, and swank-server. It stops any prior
// default server before starting the new one so repeat calls on
// different ports do not leak the previous listener and its goroutines.
func startDefaultServer(s *slip.Scope, depth int, port slip.Fixnum, host string) slip.Object {
	if defaultServer != nil {
		_ = defaultServer.Stop()
		defaultServer = nil
	}
	address := fmt.Sprintf("%s:%d", host, port)
	server := NewServer(s)
	if err := server.Start(address); err != nil {
		slip.ErrorPanic(s, depth, "failed to start swank server: %v", err)
	}
	defaultServer = server
	return &SwankServerInstance{server: server}
}

// parsePortKeyword extracts :port from keyword args, defaulting to 4005.
func parsePortKeyword(s *slip.Scope, depth int, f slip.Object, args slip.List) slip.Fixnum {
	for i := 0; i < len(args); i += 2 {
		key, ok := args[i].(slip.Symbol)
		if !ok || i+1 >= len(args) {
			continue
		}
		if key == ":port" {
			if p, ok := args[i+1].(slip.Fixnum); ok {
				return p
			}
		}
	}
	return 4005
}

// defCreateServer defines create-server — the standard SLIME way to start a server.
func defCreateServer() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CreateServer{Function: slip.Function{Name: "create-server", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "create-server",
			Args: []*slip.DocArg{
				{Name: "&key"},
				{Name: "port", Type: "fixnum", Text: "port to listen on (default 4005)."},
				{Name: "dont-close", Type: "boolean", Text: "ignored (always persistent)."},
			},
			Return: "swank-server",
			Text: `__create-server__ starts a Swank server for SLIME integration.
This is the standard SLIME entry point. Equivalent to __swank-server__.`,
			Examples: []string{
				`(create-server :port 4005) => #<swank-server :4005>`,
			},
		}, &Pkg)
}

// CreateServer represents the create-server function.
type CreateServer struct {
	slip.Function
}

func (f *CreateServer) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 4)
	port := parsePortKeyword(s, depth, f, args)
	return startDefaultServer(s, depth, port, "")
}

// defStartServer defines start-server — writes port to a file (SLIME convention).
func defStartServer() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StartServer{Function: slip.Function{Name: "start-server", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "start-server",
			Args: []*slip.DocArg{
				{Name: "port-file", Type: "string", Text: "path to write the port number (ignored)."},
				{Name: "&key"},
				{Name: "port", Type: "fixnum", Text: "port to listen on (default 4005)."},
			},
			Return: "swank-server",
			Text: `__start-server__ starts a Swank server. The port-file argument is
accepted for SLIME compatibility but ignored. Equivalent to __create-server__.`,
			Examples: []string{
				`(start-server "/tmp/swank-port" :port 4005) => #<swank-server :4005>`,
			},
		}, &Pkg)
}

// StartServer represents the start-server function.
type StartServer struct {
	slip.Function
}

func (f *StartServer) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 6)
	// Skip the port-file arg if present (first non-keyword arg).
	kargs := args
	if len(args) > 0 {
		if _, isStr := args[0].(slip.String); isStr {
			kargs = args[1:]
		}
	}
	port := parsePortKeyword(s, depth, f, kargs)
	return startDefaultServer(s, depth, port, "")
}

// defStopServer defines stop-server — stops a server on a given port.
func defStopServer() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StopServer{Function: slip.Function{Name: "stop-server", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "stop-server",
			Args: []*slip.DocArg{
				{Name: "port", Type: "fixnum", Text: "port of server to stop (default: stop the default server)."},
			},
			Return: "nil",
			Text:   `__stop-server__ stops the Swank server. Equivalent to __swank-stop__.`,
			Examples: []string{
				`(stop-server 4005) => nil`,
			},
		}, &Pkg)
}

// StopServer represents the stop-server function.
type StopServer struct {
	slip.Function
}

func (f *StopServer) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 1)
	if defaultServer != nil {
		_ = defaultServer.Stop()
		defaultServer = nil
	}
	return nil
}

// defRestartServer defines restart-server — stops then starts.
func defRestartServer() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RestartServer{Function: slip.Function{Name: "restart-server", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "restart-server",
			Args: []*slip.DocArg{
				{Name: "&key"},
				{Name: "port", Type: "fixnum", Text: "port to listen on (default 4005)."},
			},
			Return: "swank-server",
			Text:   `__restart-server__ stops the current server and starts a new one.`,
			Examples: []string{
				`(restart-server :port 4005) => #<swank-server :4005>`,
			},
		}, &Pkg)
}

// RestartServer represents the restart-server function.
type RestartServer struct {
	slip.Function
}

func (f *RestartServer) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 4)
	port := parsePortKeyword(s, depth, f, args)
	return startDefaultServer(s, depth, port, "")
}

// defSetupServer defines setup-server — lower-level server setup.
func defSetupServer() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SetupServer{Function: slip.Function{Name: "setup-server", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "setup-server",
			Args: []*slip.DocArg{
				{Name: "port", Type: "fixnum", Text: "port to listen on."},
			},
			Return: "swank-server",
			Text: `__setup-server__ sets up a Swank server on the given port.
This is the lower-level entry point. Most users should use __create-server__ instead.`,
			Examples: []string{
				`(setup-server 4005) => #<swank-server :4005>`,
			},
		}, &Pkg)
}

// SetupServer represents the setup-server function.
type SetupServer struct {
	slip.Function
}

func (f *SetupServer) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	port := slip.Fixnum(4005)
	if p, ok := args[0].(slip.Fixnum); ok {
		port = p
	}
	return startDefaultServer(s, depth, port, "")
}
