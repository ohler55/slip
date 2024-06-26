// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"io"
	"strconv"
	"unsafe"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PrintUnreadableObject{Function: slip.Function{Name: "print-unreadable-object", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "print-unreadable-object",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: `The object to write to stream.`,
				},
				{
					Name: "stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Return: "object",
			Text:   `__print-unreadable-object__ write to the _stream_.`,
			Examples: []string{
				`(print-unreadable-object *package*) => #<package 12345>`,
			},
		}, &slip.CLPkg)
}

// PrintUnreadableObject represents the print-unreadable-object function.
type PrintUnreadableObject struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PrintUnreadableObject) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	w, ok := args[1].(io.Writer)
	if !ok {
		slip.PanicType("stream", args[1], "output-stream")
	}
	if args[0] != nil {
		var b []byte
		b = fmt.Appendf(b, "#<%s ", args[0].Hierarchy()[0])
		b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(&args[0]))), 16)
		b = append(b, '>')
		if _, err := w.Write(b); err != nil {
			panic(err)
		}
	}
	return nil
}
