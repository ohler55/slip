// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import (
	"syscall"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSocketMakeStream() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SocketMakeStream{Function: slip.Function{Name: "socket-make-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "socket-make-stream",
			Args: []*slip.DocArg{
				{
					Name: "socket",
					Type: "socket",
					Text: "to create a stream for.",
				},
				{Name: "&key"},
				{
					Name: "input",
					Type: "boolean",
					Text: "indicates the stream should support reads.",
				},
				{
					Name: "output",
					Type: "boolean",
					Text: "indicates the stream should support writes.",
				},
				{
					Name: "timeout",
					Type: "real|nil",
					Text: "if non-nil then the number of seconds for the read timeout.",
				},
			},
			Return: "nil|",
			Text:   `__socket-make-stream__ makes a stream from the _socket_ instance.`,
			Examples: []string{
				`(let ((sock (make-instance 'socket :socket 5)))`,
				`  (socket-make-stream sock :input t)) => #<stream>`,
			},
		}, &Pkg)
}

// SocketMakeStream represents the socket-make-stream function.
type SocketMakeStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SocketMakeStream) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 7)
	self, ok := args[0].(*flavors.Instance)
	if !ok || !self.IsA("socket") {
		slip.TypePanic(s, depth, "socket", args[0], "socket")
	}
	return makeStream(self, args[1:])
}

type socketMakeStreamCaller struct{}

func (caller socketMakeStreamCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.CheckSendArgCount(s, depth, self, ":make-stream", args, 0, 6)

	return makeStream(self, args)
}

func (caller socketMakeStreamCaller) FuncDocs() *slip.FuncDoc {
	md := methodDocFromFunc(":make-stream", "socket-make-stream", &Pkg)
	md.Examples[len(md.Examples)-1] = `  (send sock :make-stream :input t)) => #<stream>`
	return md
}

func makeStream(self *flavors.Instance, args slip.List) (result slip.Object) {
	var (
		in  bool
		out bool
	)
	fd, ok := self.Any.(int)
	if !ok {
		return
	}
	if value, _ := slip.GetArgsKeyValue(args, slip.Symbol(":input")); value != nil {
		in = true
	}
	if value, _ := slip.GetArgsKeyValue(args, slip.Symbol(":output")); value != nil {
		out = true
	}
	if value, has := slip.GetArgsKeyValue(args, slip.Symbol(":timeout")); has && value != nil {
		if err := setSockoptTime(fd, syscall.SO_RCVTIMEO, value); err != nil {
			panic(err)
		}
	}
	switch {
	case in && out:
		result = &slip.IOStream{RW: fdRW(fd)}
	case in:
		result = &slip.InputStream{Reader: fdRW(fd)}
	case out:
		result = &slip.OutputStream{Writer: fdRW(fd)}
	default:
		slip.NewPanic(":input or :output must be true")
	}
	return
}
