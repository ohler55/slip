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
			f := Column{Function: slip.Function{Name: "parquet-column", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "parquet-column",
			Args: []*slip.DocArg{
				{
					Name: "reader",
					Type: "parquet-reader-flavor instance",
					Text: "The parquet reader to get the column of.",
				},
				{
					Name: "id",
					Type: "fixnum|string|symbol",
					Text: "The column identifier as either the column ID or name.",
				},
			},
			Return: "list",
			Text: `__parquet-column__ returns the specified column of a parquet reader.

This is the same as sending _:column_ to an instance of the _parquet-reader-flavor_ except
that none of the _:column_ daemons are called.`,
			Examples: []string{
				`(setq reader (parquet-open "sample.parquet"))`,
				`(parquet-column reader 0) => (1 2 3)`,
			},
		}, &Pkg)
}

// Column represents the parquet-column function.
type Column struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Column) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
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
	if cr, _ := findColReader(fr, schem, args[1]); cr != nil {
		result = readColumn(cr)
	}
	return
}
