// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WithOpenFile{
				Open: Open{
					Function: slip.Function{Name: "with-open-file", Args: args, SkipEval: []bool{true}},
				},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "with-open-file",
			Args: []*slip.DocArg{
				{
					Name: "args",
					Type: "list",
					Text: `A list of _stream_, _filepath_, and _options*_. The _options*_ are the same as
the options to the __open__ function. The _stream_ must be a symbol that is bound to the opened _file-stream_.`,
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "form",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text: `__with-open-file__ evaluates the _forms_ after opening the _filepath_ and binding
_stream_ to the opened _file-stream_.`,
			Examples: []string{
				`(with-open-file`,
				`  (file "test.json" :direction :input) (make-instance bag-flavor :read file)) => #<bag-flavor 1234>`,
			},
		}, &slip.CLPkg)
}

// WithOpenFile represents the with-open-file function.
type WithOpenFile struct {
	Open
}

// Call the function with the arguments provided.
func (f *WithOpenFile) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, -1)
	fargs, ok := args[0].(slip.List)
	if !ok || len(fargs) < 2 {
		slip.PanicType("args", args[0], "list of (symbol filepath options*)")
	}
	var sym slip.Symbol
	if sym, ok = fargs[0].(slip.Symbol); !ok {
		slip.PanicType("stream", fargs[0], "symbol")
	}
	d2 := depth + 1
	fargs = fargs[1:]
	eargs := make(slip.List, len(fargs))
	for i := range fargs {
		eargs[i] = slip.EvalArg(s, fargs, i, d2)
	}
	file := f.openFile(eargs)
	defer func() {
		if closer, _ := file.(io.Closer); closer != nil {
			_ = closer.Close()
		}
	}()
	s2 := s.NewScope()
	s2.Let(sym, file)
	args = args[1:]
	for i := range args {
		result = slip.EvalArg(s2, args, i, d2)
	}
	return
}
