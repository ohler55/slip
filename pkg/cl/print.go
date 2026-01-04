// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Print{Function: slip.Function{Name: "print", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "print",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The _object_ to be printed.",
				},
				{Name: "&optional"},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Return: "object",
			Text: `__print__ writes a string representation of the _object_ to the provided _output-stream_.
Output is produced as if _*print-escape*_ is _false_. A newline is prepended to the output and a space is
appended to the output.
If the _output-stream_ is not provided then the _*standard-output*_ is used. The _object_ is returned.`,
			Examples: []string{
				"(print 123) => 123 ;; \n123 is written",
			},
		}, &slip.CLPkg)
}

// Print represents the print function.
type Print struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Print) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	obj := args[0]
	w := slip.StandardOutput.(io.Writer)
	ss, _ := slip.StandardOutput.(slip.Stream)
	if 1 < len(args) {
		var ok bool
		ss, _ = args[1].(slip.Stream)
		if w, ok = args[1].(io.Writer); !ok {
			slip.TypePanic(s, depth, "print output-stream", args[1], "output-stream")
		}
	}
	// // Check for StructureObject with custom print function
	// if so, ok := obj.(*StructureObject); ok {
	// 	if pf := so.Type.printFunc; pf != nil {
	// 		if _, err := w.Write([]byte{'\n'}); err != nil {
	// 			slip.StreamPanic(s, depth, ss, "print write failed. %s", err)
	// 		}
	// 		// :print-function signature: (object stream depth)
	// 		pf.Call(s, slip.List{so, ss, slip.Fixnum(0)}, depth+1)
	// 		if _, err := w.Write([]byte{' '}); err != nil {
	// 			slip.StreamPanic(s, depth, ss, "print write failed. %s", err)
	// 		}
	// 		return obj
	// 	}
	// 	if po := so.Type.printObject; po != nil {
	// 		if _, err := w.Write([]byte{'\n'}); err != nil {
	// 			slip.StreamPanic(s, depth, ss, "print write failed. %s", err)
	// 		}
	// 		// :print-object signature: (object stream)
	// 		po.Call(s, slip.List{so, ss}, depth+1)
	// 		if _, err := w.Write([]byte{' '}); err != nil {
	// 			slip.StreamPanic(s, depth, ss, "print write failed. %s", err)
	// 		}
	// 		return obj
	// 	}
	// }
	p := *slip.DefaultPrinter()
	p.ScopedUpdate(s)
	p.Escape = true
	b := []byte{'\n'}
	if sa, ok := obj.(slip.ScopedAppender); ok {
		b = sa.ScopedAppend(b, s, &p, 0)
	} else {
		b = p.Append(b, obj, 0)
	}
	b = append(b, ' ')
	if _, err := w.Write(b); err != nil {
		slip.StreamPanic(s, depth, ss, "print write failed. %s", err)
	}
	return obj
}
