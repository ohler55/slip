// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"github.com/apache/arrow/go/v13/parquet/file"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var columnFlavor *flavors.Flavor

func init() {
	columnFlavor = flavors.DefFlavor("parquet-column-flavor",
		map[string]slip.Object{},
		nil,
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A parquet column represents a parquet row group column.`),
			},
		},
	)
	columnFlavor.Final = true
	columnFlavor.DefMethod(":type", "", columnTypeCaller(true))
	// TBD values
	// TBD walk or map or each or each-value
	//  need to check the actual type of the col reader
}

type columnTypeCaller bool

func (caller columnTypeCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if ccr, ok := obj.Any.(file.ColumnChunkReader); ok {
		result = slip.String(ccr.Type().String())
	}
	return
}

func (caller columnTypeCaller) Docs() string {
	return `__:type__ => _string_

Returns the type of the row column.
`
}

func makeColumn(ccr file.ColumnChunkReader) (inst *flavors.Instance) {
	inst = columnFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = ccr
	return
}
