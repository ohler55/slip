// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive

import (
	"fmt"
	"net"
	"sync"
	"sync/atomic"

	"github.com/ohler55/slip"
)

// Server manages LSP connections for the Alive extension.
type Server struct {
	listener    net.Listener
	connections sync.Map // map[int64]*Connection
	nextConnID  atomic.Int64
	baseScope   *slip.Scope
	running     atomic.Bool
	address     string
}

// NewServer creates a new Alive LSP server with the given base scope.
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
				fmt.Printf("alive: accept error: %v\n", err)
			}
			continue
		}

		id := s.nextConnID.Add(1)
		aliveConn := NewConnection(id, conn, s)
		s.connections.Store(id, aliveConn)

		go aliveConn.Run()
	}
}

// removeConnection removes a connection from the server's tracking.
func (s *Server) removeConnection(id int64) {
	s.connections.Delete(id)
}

// defAliveServer defines the alive-server function.
func defAliveServer() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AliveServer{Function: slip.Function{Name: "alive-server", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "alive-server",
			Args: []*slip.DocArg{
				{
					Name: "&key",
				},
				{
					Name: "port",
					Type: "fixnum",
					Text: "port to listen on (default 4006).",
				},
				{
					Name: "host",
					Type: "string",
					Text: "host/interface to bind to (default all interfaces).",
				},
			},
			Return: "alive-server",
			Text: `__alive-server__ starts an LSP server for VSCode's Alive extension.
The server listens for connections from VSCode and handles JSON-RPC requests
for evaluation, completion, and other IDE features.

Once started, configure VSCode's Alive extension to connect to the server.`,
			Examples: []string{
				`(alive-server) => #<alive-server :4006>`,
				`(alive-server :port 5006) => #<alive-server :5006>`,
			},
		}, &Pkg)
}

// AliveServer represents the alive-server function.
type AliveServer struct {
	slip.Function
}

// Call starts the Alive LSP server.
func (f *AliveServer) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 4)

	port := slip.Fixnum(4006)
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
		slip.ErrorPanic(s, depth, "failed to start alive server: %v", err)
	}

	defaultServer = server
	return &AliveServerInstance{server: server}
}

// AliveServerInstance represents a running Alive server.
type AliveServerInstance struct {
	server *Server
}

func (s *AliveServerInstance) String() string {
	return fmt.Sprintf("#<alive-server %s>", s.server.Addr())
}

func (s *AliveServerInstance) Append(b []byte) []byte {
	return append(b, s.String()...)
}

func (s *AliveServerInstance) Simplify() any {
	return s.String()
}

func (s *AliveServerInstance) Equal(other slip.Object) bool {
	if o, ok := other.(*AliveServerInstance); ok {
		return s.server == o.server
	}
	return false
}

func (s *AliveServerInstance) Hierarchy() []slip.Symbol {
	return []slip.Symbol{"alive-server", slip.TrueSymbol}
}

func (s *AliveServerInstance) Eval(scope *slip.Scope, depth int) slip.Object {
	return s
}

// defAliveStop defines the alive-stop function.
func defAliveStop() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AliveStop{Function: slip.Function{Name: "alive-stop", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "alive-stop",
			Args:   []*slip.DocArg{},
			Return: "nil",
			Text:   `__alive-stop__ stops the default Alive LSP server.`,
			Examples: []string{
				`(alive-stop) => nil`,
			},
		}, &Pkg)
}

// AliveStop represents the alive-stop function.
type AliveStop struct {
	slip.Function
}

// Call stops the default Alive server.
func (f *AliveStop) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 0)

	if defaultServer != nil {
		_ = defaultServer.Stop()
		defaultServer = nil
	}
	return nil
}
