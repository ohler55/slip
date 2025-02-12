// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := BroadcastStreamStreams{Function: slip.Function{Name: "broadcast-stream-streams", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "broadcast-stream-streams",
			Args: []*slip.DocArg{
				{
					Name: "broadcast-stream",
					Type: "broadcast-stream",
					Text: "The broadcast-stream to return the component strems of.",
				},
			},
			Return: "list",
			Text:   `__broadcast-stream-streams__ returns the component streams of _broadcast-stream_`,
			Examples: []string{
				`(let* ((ss1 (make-string-output-stream))`,
				`       (ss2 (make-string-output-stream))`,
				`       (bs (make-broadcast-stream ss1 ss2)))`,
				` (broadcast-stream-streams bs)) => (#<string-stream> #<string-stream>)`,
			},
		}, &slip.CLPkg)
}

// BroadcastStreamStreams represents the broadcast-stream-streams function.
type BroadcastStreamStreams struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *BroadcastStreamStreams) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	bs, ok := args[0].(BroadcastStream)
	if !ok {
		slip.PanicType("broadcast-stream", args[0], "broadcast-stream")
	}
	streams := make(slip.List, len(bs))
	for i, stream := range bs {
		streams[i] = stream
	}
	return streams
}
