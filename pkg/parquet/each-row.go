// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"github.com/apache/arrow/go/v14/parquet/pqarrow"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := EachRow{Function: slip.Function{Name: "parquet-each-row", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "parquet-each-row",
			Args: []*slip.DocArg{
				{
					Name: "reader",
					Type: "parquet-reader-flavor instance",
					Text: "The parquet reader to get the each-row of.",
				},
				{
					Name: "function",
					Type: "function",
					Text: "The funcion to call for each of the reader.",
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
			Return: "nil",
			Text: `__parquet-each-row__ calls the _function_ for each row in the parquet
reader in the _format_ specified and with the columns identified by the _column-ids_.

This is the same as sending _:each-row_ to an instance of the _parquet-reader-flavor_ except
that none of the _:each-row_ daemons are called.`,
			Examples: []string{
				`(setq reader (parquet-open "sample.parquet") rows nil)`,
				`(parquet-each-row reader (lambda (row) (setq rows (add rows row))) :assoc) => nil`,
				`  ;; rows will contain an assoc list of all rows`,
			},
		}, &Pkg)
}

// EachRow represents the parquet-each-row function.
type EachRow struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *EachRow) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 3, 4)
	inst, ok := args[0].(*flavors.Instance)
	if !ok {
		slip.PanicType("reader", args[0], "parquet-reader-flavor instance")
	}
	var fr *pqarrow.FileReader
	if fr, ok = inst.Any.(*pqarrow.FileReader); !ok {
		slip.PanicType("reader", args[0], "parquet-reader-flavor instance")
	}
	d2 := depth + 1
	fun := cl.ResolveToCaller(s, args[1], d2)
	var ids slip.Object
	if 3 < len(args) {
		ids = args[3]
	}
	rr := newRowReader(fr, ids, args[2])
	for {
		row := rr.next()
		if row == nil {
			break
		}
		_ = fun.Call(s, slip.List{row}, d2)
	}
	return nil
}
