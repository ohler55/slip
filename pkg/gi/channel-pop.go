// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ChannelPop{Function: slip.Function{Name: "channel-pop", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "channel-pop",
			Args: []*slip.DocArg{
				{
					Name: "channel",
					Type: "channel",
					Text: "The channel to pop an object off of.",
				},
			},
			Return: "object",
			Text:   `__channel-pop__ pops a value off a channel.`,
			Examples: []string{
				`(setq queue (make-channel 10)) => #<channel 12345>`,
				`(channel-push  queue 10)`,
				`(channel-pop  queue) => 10`,
			},
		}, &Pkg)
}

// ChannelPop represents the channelPop function.
type ChannelPop struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ChannelPop) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	ch, ok := args[0].(Popper)
	if !ok {
		slip.PanicType("channel", args[0], "channel")
	}
	return ch.Pop()
}
