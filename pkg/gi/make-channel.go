// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeChannel{Function: slip.Function{Name: "make-channel", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-channel",
			Args: []*slip.DocArg{
				{
					Name: "size",
					Type: "fixnum",
					Text: "The channel capacity as a non-negative fixnum.",
				},
			},
			Return: "channel",
			Text:   `__make-channel__ make a channel.`,
			Examples: []string{
				`(make-channel 10) => #<channel 12345>`,
			},
		}, &GiPkg)
}

// MakeChannel represents the makeChannel function.
type MakeChannel struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeChannel) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	size, ok := args[0].(slip.Fixnum)
	if !ok || int(size) < 0 {
		slip.PanicType("size", args[0], "non-negative fixnum")
	}
	return Channel(make(chan slip.Object, int(size)))
}
