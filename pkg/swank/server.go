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

	address := fmt.Sprintf("%s:%d", host, port)

	server := NewServer(s)
	if err := server.Start(address); err != nil {
		slip.ErrorPanic(s, depth, "failed to start swank server: %v", err)
	}

	defaultServer = server
	return &SwankServerInstance{server: server}
}

// SwankServerInstance represents a running Swank server.
type SwankServerInstance struct {
	server *Server
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
