// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"context"

	"github.com/apache/arrow/go/v14/parquet/pqarrow"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Columns{Function: slip.Function{Name: "parquet-columns", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "parquet-columns",
			Args: []*slip.DocArg{
				{
					Name: "reader",
					Type: "parquet-reader-flavor instance",
					Text: "The parquet reader to get the columns of.",
				},
			},
			Return: "list",
			Text: `__parquet-columns__ returns the columns of a parquet reader.

This is the same as sending _:columns_ to an instance of the _parquet-reader-flavor_ except
that none of the _:columns_ daemons are called.`,
			Examples: []string{
				`(setq reader (parquet-open "sample.parquet"))`,
				`(parquet-columns reader) => ((1 2 3) ("a" "b" "c"))`,
			},
		}, &Pkg)
}

// Columns represents the parquet-columns function.
type Columns struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Columns) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	inst, ok := args[0].(*flavors.Instance)
	if !ok {
		slip.PanicType("reader", args[0], "parquet-reader-flavor instance")
	}
	var fr *pqarrow.FileReader
	if fr, ok = inst.Any.(*pqarrow.FileReader); !ok {
		slip.PanicType("reader", args[0], "parquet-reader-flavor instance")
	}
	schem, err := fr.Schema()
	if err != nil {
		panic(err)
	}
	columns := make(slip.List, schem.NumFields())
	for i := 0; i < schem.NumFields(); i++ {
		if cr, _ := fr.GetColumn(context.Background(), i); cr != nil {
			columns[i] = readColumn(cr)
		}
	}
	return columns
}
