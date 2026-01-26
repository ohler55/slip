// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"fmt"
	"net"
	"sync"
	"sync/atomic"

	"github.com/ohler55/slip"
)

// Server manages Slynk connections for SLY integration.
type Server struct {
	listener    net.Listener
	connections sync.Map // map[int64]*Connection
	nextConnID  atomic.Int64
	baseScope   *slip.Scope
	running     atomic.Bool
	address     string
}

// NewServer creates a new Slynk server with the given base scope.
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
				LogError("accept error: %v", err)
			}
			continue
		}

		id := s.nextConnID.Add(1)
		slynkConn := NewConnection(id, conn, s)
		s.connections.Store(id, slynkConn)

		go slynkConn.Run()
	}
}

// removeConnection removes a connection from the server's tracking.
func (s *Server) removeConnection(id int64) {
	s.connections.Delete(id)
}

// defSlynkServer defines the slynk-server function.
func defSlynkServer() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SlynkServer{Function: slip.Function{Name: "slynk-server", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "slynk-server",
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
			Return: "slynk-server",
			Text: `__slynk-server__ starts a Slynk server for SLY integration.
The server listens for connections from Emacs SLY and handles RPC requests
for evaluation, completion, and other IDE features.

Slynk supports multiple concurrent REPLs via channels, enhanced flex-completion,
and other features not available in Swank.

Once started, connect from Emacs with:
  M-x sly-connect RET localhost RET 4005 RET`,
			Examples: []string{
				`(slynk-server) => #<slynk-server :4005>`,
				`(slynk-server :port 5005) => #<slynk-server :5005>`,
			},
		}, &Pkg)
}

// SlynkServer represents the slynk-server function.
type SlynkServer struct {
	slip.Function
}

// Call starts the Slynk server.
func (f *SlynkServer) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
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
		slip.ErrorPanic(s, depth, "failed to start slynk server: %v", err)
	}

	defaultServer = server
	return &SlynkServerInstance{server: server}
}

// SlynkServerInstance represents a running Slynk server.
type SlynkServerInstance struct {
	server *Server
}

func (s *SlynkServerInstance) String() string {
	return fmt.Sprintf("#<slynk-server %s>", s.server.Addr())
}

func (s *SlynkServerInstance) Append(b []byte) []byte {
	return append(b, s.String()...)
}

func (s *SlynkServerInstance) Simplify() any {
	return s.String()
}

func (s *SlynkServerInstance) Equal(other slip.Object) bool {
	if o, ok := other.(*SlynkServerInstance); ok {
		return s.server == o.server
	}
	return false
}

func (s *SlynkServerInstance) Hierarchy() []slip.Symbol {
	return []slip.Symbol{"slynk-server", slip.TrueSymbol}
}

func (s *SlynkServerInstance) Eval(scope *slip.Scope, depth int) slip.Object {
	return s
}

// defSlynkStop defines the slynk-stop function.
func defSlynkStop() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SlynkStop{Function: slip.Function{Name: "slynk-stop", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "slynk-stop",
			Args:   []*slip.DocArg{},
			Return: "nil",
			Text:   `__slynk-stop__ stops the default Slynk server.`,
			Examples: []string{
				`(slynk-stop) => nil`,
			},
		}, &Pkg)
}

// SlynkStop represents the slynk-stop function.
type SlynkStop struct {
	slip.Function
}

// Call stops the default Slynk server.
func (f *SlynkStop) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 0)

	if defaultServer != nil {
		_ = defaultServer.Stop()
		defaultServer = nil
	}
	return nil
}
