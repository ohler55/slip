// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"errors"
	"io"

	"github.com/ohler55/slip"
)

func defCopyStreamToStream() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CopyStreamToStream{Function: slip.Function{Name: "copy-stream-to-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "copy-stream-to-stream",
			Args: []*slip.DocArg{
				{
					Name: "input",
					Type: "input-stream",
					Text: "The stream to read from.",
				},
				{
					Name: "output",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
				{Name: "&key"},
				{
					Name: "buffer-size",
					Type: "fixnum",
					Text: "A suggested buffer size.",
				},
			},
			Return: "nil",
			Text:   `__copy-stream-to-stream__ copies from one stream to another.`,
		}, &Pkg)
}

// CopyStreamToStream represents the copy-stream-to-stream function.
type CopyStreamToStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CopyStreamToStream) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 4)
	r, ok := args[0].(io.Reader)
	if !ok {
		slip.TypePanic(s, depth, "input", args[0], "input-stream")
	}
	var w io.Writer
	if w, ok = args[1].(io.Writer); !ok {
		slip.TypePanic(s, depth, "output", args[1], "output-stream")
	}
	bufSize := 16384 // default size
	if 2 < len(args) {
		if v, has := slip.GetArgsKeyValue(args[2:], slip.Symbol(":buffer-size")); has {
			if num, ok2 := v.(slip.Fixnum); ok2 {
				if 0 < num && num <= 65536 {
					bufSize = int(num)
				}
			} else {
				slip.TypePanic(s, depth, ":buffer-size", v, "fixnum")
			}
		}
	}
	buf := make([]byte, bufSize)
	for {
		cnt, err := r.Read(buf)
		if 0 < cnt {
			if _, err2 := w.Write(buf[:cnt]); err2 != nil {
				stream, _ := args[1].(slip.Stream)
				slip.StreamPanic(s, depth, stream, "Write error. %s", err)
			}
		}
		if err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			stream, _ := args[0].(slip.Stream)
			slip.StreamPanic(s, depth, stream, "Read error. %s", err)
		}

	}
	return nil
}
