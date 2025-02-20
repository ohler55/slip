// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ConcatenatedStreamStreams{Function: slip.Function{Name: "concatenated-stream-streams", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "concatenated-stream-streams",
			Args: []*slip.DocArg{
				{
					Name: "concatenated-stream",
					Type: "concatenated-stream",
					Text: "The concatenated-stream to return the component streams of.",
				},
			},
			Return: "list",
			Text:   `__concatenated-stream-streams__ returns the component streams of _concatenated-stream_`,
			Examples: []string{
				`(let* ((ss1 (make-string-output-stream))`,
				`       (ss2 (make-string-output-stream))`,
				`       (bs (make-concatenated-stream ss1 ss2)))`,
				` (concatenated-stream-streams bs)) => (#<string-stream> #<string-stream>)`,
			},
		}, &slip.CLPkg)
}

// ConcatenatedStreamStreams represents the concatenated-stream-streams function.
type ConcatenatedStreamStreams struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ConcatenatedStreamStreams) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	cs, ok := args[0].(ConcatenatedStream)
	if !ok {
		slip.PanicType("concatenated-stream", args[0], "concatenated-stream")
	}
	streams := make(slip.List, len(cs)-1)
	for i, stream := range cs[1:] {
		streams[i] = stream
	}
	return streams
}
