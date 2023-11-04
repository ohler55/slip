// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"github.com/apache/arrow/go/v14/parquet/pqarrow"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CreatedBy{Function: slip.Function{Name: "parquet-created-by", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "parquet-created-by",
			Args: []*slip.DocArg{
				{
					Name: "reader",
					Type: "parquet-reader-flavor instance",
					Text: "The parquet reader to get the created-by of.",
				},
			},
			Return: "nil",
			Text: `__parquet-created-by__ created-bys a parquet reader.

This is the same as sending _:created-by_ to an instance of the _parquet-reader-flavor_ except
that none of the _:created-by_ daemons are called.`,
			Examples: []string{
				`(setq reader (parquet-open "sample.parquet"))`,
				`(parquet-created-by reader) => nil`,
			},
		}, &Pkg)
}

// Created-By represents the parquet-created-by function.
type CreatedBy struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CreatedBy) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	inst, ok := args[0].(*flavors.Instance)
	if !ok {
		slip.PanicType("reader", args[0], "parquet-reader-flavor instance")
	}
	var fr *pqarrow.FileReader
	if fr, ok = inst.Any.(*pqarrow.FileReader); !ok {
		slip.PanicType("reader", args[0], "parquet-reader-flavor instance")
	}
	return slip.String(fr.ParquetReader().MetaData().GetCreatedBy())
}
