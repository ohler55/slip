// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"github.com/apache/arrow/go/v14/arrow/memory"
	"github.com/apache/arrow/go/v14/parquet/file"
	"github.com/apache/arrow/go/v14/parquet/pqarrow"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Open{Function: slip.Function{Name: "parquet-open", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "parquet-open",
			Args: []*slip.DocArg{
				{
					Name: "file",
					Type: "string",
					Text: "The parquet file to read.",
				},
			},
			Return: "instance",
			Text: `__parquet-open__ opens a parquet file and returns a _parquet-reader-flavor_ instance.

This is the same as the calling _make-instance_ of the _parquet-reader-flavor_ with the _:file_ init
option of _:file_ except that none of the _:init_ daemons are called.`,
			Examples: []string{
				`(parquet-open "sample.parquet") => #<parquet-reader-flavor 1234>`,
			},
		}, &Pkg)
}

// Open represents the parquet-open function.
type Open struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Open) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("file", args[0], "string")
	}
	inst := readerFlavor.MakeInstance().(*flavors.Instance)
	initReader(inst, string(path))

	return inst
}

func initReader(inst *flavors.Instance, path string) {
	var fr *pqarrow.FileReader
	pr, err := file.OpenParquetFile(path, true)
	if err == nil {
		fr, err = pqarrow.NewFileReader(pr, pqarrow.ArrowReadProperties{}, memory.DefaultAllocator)
	}
	if err != nil {
		panic(err)
	}
	inst.Any = fr
	inst.Let(slip.Symbol("filepath"), slip.String(path))
}
