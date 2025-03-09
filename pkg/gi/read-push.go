// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReadPush{Function: slip.Function{Name: "read-push", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "read-push",
			Args: []*slip.DocArg{
				{
					Name: "input-stream",
					Type: "input-stream",
					Text: "The stream to read from.",
				},
				{
					Name: "channel",
					Type: "channel",
					Text: "The channel to call for push expression to when read.",
				},
			},
			Return: "nil",
			Text:   `__read-push__ pushes the s-expression read from the _input-stream_ on to _channel_.`,
			Examples: []string{
				`(let ((chan (make-channel 2)))`,
				` (read-push (make-string-input-stream "(1 2 3)") chan)`,
				` (channel-pop chan)) => (1 2 3)`,
			},
		}, &Pkg)
}

// ReadPush represents the read-push function.
type ReadPush struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ReadPush) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	r, ok := args[0].(io.Reader)
	if !ok {
		slip.PanicType("input-stream", args[0], "input-stream")
	}
	var channel Channel
	if channel, ok = args[1].(Channel); !ok {
		slip.PanicType("channel", args[1], "channel")
	}
	slip.ReadStreamPush(r, s, channel)

	return nil
}
