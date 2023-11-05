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
			f := Rows{Function: slip.Function{Name: "parquet-rows", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "parquet-rows",
			Args: []*slip.DocArg{
				{
					Name: "reader",
					Type: "parquet-reader-flavor instance",
					Text: "The parquet reader to get the rows of.",
				},
				{
					Name: "format",
					Type: "keyword",
					Text: `The format of each row. Valid values are _:list_ for a list of
column values in order or _:assoc_ for column names as the car and the values as the cdr
of each element in an assoc list for each key value pair.`,
				},
				{Name: "&optional"},
				{
					Name: "column-ids",
					Type: "list",
					Text: `A list of the column indexes or names for the columns to get values
from. The default is all columns`,
				},
			},
			Return: "list",
			Text: `__parquet-rows__ returns the rows of a parquet reader in the _format_
specified and with the columns identified by the _column-ids_.

This is the same as sending _:rows_ to an instance of the _parquet-reader-flavor_ except
that none of the _:rows_ daemons are called.`,
			Examples: []string{
				`(setq reader (parquet-open "sample.parquet"))`,
				`(parquet-rows reader :assoc '(0 2)) => ((("a" . 1)("c" . t)) (("a" . 2) ("c" . nil)))`,
			},
		}, &Pkg)
}

// Rows represents the parquet-rows function.
type Rows struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Rows) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 3)
	inst, ok := args[0].(*flavors.Instance)
	if !ok {
		slip.PanicType("reader", args[0], "parquet-reader-flavor instance")
	}
	var fr *pqarrow.FileReader
	if fr, ok = inst.Any.(*pqarrow.FileReader); !ok {
		slip.PanicType("reader", args[0], "parquet-reader-flavor instance")
	}
	rowCnt := fr.ParquetReader().NumRows()
	rows := make(slip.List, 0, rowCnt)
	var ids slip.Object
	if 2 < len(args) {
		ids = args[2]
	}
	rr := newRowReader(fr, ids, args[1])
	for {
		row := rr.next()
		if row == nil {
			break
		}
		rows = append(rows, row)
	}
	return rows
}
