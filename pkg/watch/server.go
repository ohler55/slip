// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"net"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	serverFlavor *flavors.Flavor
)

func init() {
	Pkg.Initialize(nil)
	serverFlavor = flavors.DefFlavor("watch-server", map[string]slip.Object{}, nil,
		slip.List{
			slip.List{
				slip.Symbol(":init-keywords"),
				slip.Symbol(":port"),
			},
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A server that listens on a port for LISP expressions. TBD.`),
			},
		},
		&Pkg,
	)
	serverFlavor.Final = true
	serverFlavor.DefMethod(":init", "", serverInitCaller{})
	serverFlavor.DefMethod(":shutdown", "", serverShutdownCaller{})
	serverFlavor.DefMethod(":connections", "", serverConnectionsCaller{})
}

type server struct {
	port     int
	listener net.Listener
}

type serverInitCaller struct{}

func (caller serverInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	var serv server
	for i := 0; i < len(args); i += 2 {
		if slip.Symbol(":port") == args[i] {
			if num, ok := args[i+1].(slip.Fixnum); ok {
				serv.port = int(num)
			} else {
				slip.PanicType(":port", args[i+1], "fixnum")
			}
		}
	}
	self.Any = &serv
	var err error
	if serv.listener, err = net.Listen("tcp", fmt.Sprintf(":%d", serv.port)); err != nil {
		panic(err)
	}
	go serv.listen()
	// TBD listen loop

	return nil
}

func (caller serverInitCaller) Docs() string {
	return `__:init__ &key _port_
   _:port_ [fixnum] the port to listen for connections on.


Sets the initial values when _make-instance_ is called.
`
}

type serverShutdownCaller struct{}

func (caller serverShutdownCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 1 < len(args) {
		flavors.PanicMethodArgChoice(self, ":shutdown", len(args), "0 or 1")
	}
	serv := self.Any.(*server)
	_ = serv.listener.Close()

	return nil
}

func (caller serverShutdownCaller) Docs() string {
	return `__:shutdown__ => _nil_


Shuts down the server.
`
}

type serverConnectionsCaller struct{}

func (caller serverConnectionsCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		flavors.PanicMethodArgChoice(self, ":connections", len(args), "0")
	}
	// TBD
	return nil
}

func (caller serverConnectionsCaller) Docs() string {
	return `__:connections__ => _list_


Returns a list of information about the current connections. The information
is an association list.
`
}

func (serv *server) listen() {
	for {
		con, err := serv.listener.Accept()
		if err != nil {
			// TBD exit quietly?
			fmt.Printf("*** accept failed with %T %s\n", err, err)
			return
		}
		// TBD handle connection better
		// create a connection object
		//  a channel and go routine
		//  pass in serv
		//  loop and collect bytes, try readOne until it completes without a partial panic
		c := connection{
			con: con,
		}
		// TBD add to serv

		go c.listen()
	}
}

type connection struct {
	con  net.Conn
	expr []byte
}

func (c *connection) listen() {
	buf := make([]byte, 4096)
	for {
		cnt, err := c.con.Read(buf)
		if err != nil {
			if !errors.Is(err, io.EOF) {
				fmt.Printf("*** read err: %s\n", err)
			}
			break
		}
		c.expr = append(c.expr, buf[:cnt]...)
		for {
			obj := c.readExpr()
			fmt.Printf("*** obj: %s\n", obj)
			// TBD put obj on method chan for eval later
			if obj == nil || len(c.expr) == 0 {
				break
			}
		}
	}
}

func (c *connection) readExpr() (obj slip.Object) {
	defer func() {
		if rec := recover(); rec != nil {
			// TBD if *slip.PartialPanic then return else panic(rec)
			//  where is that recovered
			fmt.Printf("*** rec: %T %s\n", rec, rec)
		}
	}()

	code, pos := slip.ReadOne(c.expr)
	if 0 < len(code) {
		obj = code[0]
	}
	after := len(c.expr) - pos
	if after <= 0 {
		c.expr = c.expr[:0]
		return
	}
	copy(c.expr, c.expr[pos:])
	c.expr = bytes.TrimSpace(c.expr[:after])
	return
}
