// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ChannelClose{Function: slip.Function{Name: "channel-close", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "channel-close",
			Args: []*slip.DocArg{
				{
					Name: "channel",
					Type: "channel",
					Text: "The channel to close.",
				},
			},
			Text: `__channel-close__ closes a channel.`,
			Examples: []string{
				`(setq queue (make-channel 10)) => #<channel 12345>`,
				`(channel-close queue)`,
			},
		}, &Pkg)
}

// ChannelClose represents the channelClose function.
type ChannelClose struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ChannelClose) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	ch, ok := args[0].(Channel)
	if !ok {
		slip.PanicType("channel", args[0], "channel")
	}
	close(ch)

	return slip.Novalue
}
