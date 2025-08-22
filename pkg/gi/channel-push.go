// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ChannelPush{Function: slip.Function{Name: "channel-push", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "channel-push",
			Args: []*slip.DocArg{
				{
					Name: "channel",
					Type: "channel",
					Text: "The channel to push an object to.",
				},
				{
					Name: "value",
					Type: "object",
					Text: "The value to push onto the channel.",
				},
			},
			Text: `__channel-push__ pushes a value onto a channel.`,
			Examples: []string{
				`(setq queue (make-channel 10)) => #<channel 12345>`,
				`(channel-push  queue 10)`,
				`(channel-pop  queue) => 10`,
			},
		}, &Pkg)
}

// ChannelPush represents the channelPush function.
type ChannelPush struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ChannelPush) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	ch, ok := args[0].(Channel)
	if !ok {
		slip.TypePanic(s, depth, "channel", args[0], "channel")
	}
	ch <- args[1]

	return slip.Novalue
}
