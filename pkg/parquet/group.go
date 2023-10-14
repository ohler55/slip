// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"github.com/apache/arrow/go/v13/parquet/file"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var groupFlavor *flavors.Flavor

func init() {
	groupFlavor = flavors.DefFlavor("parquet-group-flavor",
		map[string]slip.Object{},
		nil,
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A parquet group represents a parquet row group.`),
			},
		},
	)
	groupFlavor.Final = true
	groupFlavor.DefMethod(":size", "", groupSizeCaller(true))
	groupFlavor.DefMethod(":row-count", "", groupRowCountCaller(true))
	groupFlavor.DefMethod(":columns", "", groupColumnsCaller(true))
}

type groupSizeCaller bool

func (caller groupSizeCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if rgr, ok := obj.Any.(*file.RowGroupReader); ok {
		result = slip.Fixnum(rgr.ByteSize())
	}
	return
}

func (caller groupSizeCaller) Docs() string {
	return `__:size__ => _fixnum_

Returns the size of the row group in bytes.
`
}

type groupRowCountCaller bool

func (caller groupRowCountCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if rgr, ok := obj.Any.(*file.RowGroupReader); ok {
		result = slip.Fixnum(rgr.NumRows())
	}
	return
}

func (caller groupRowCountCaller) Docs() string {
	return `__:row-count__ => _fixnum_

Returns the number of rows of the row group.
`
}

type groupColumnsCaller bool

func (caller groupColumnsCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if rgr, ok := obj.Any.(*file.RowGroupReader); ok {
		colCnt := rgr.NumColumns()
		cols := make(slip.List, colCnt)
		for i := 0; i < colCnt; i++ {
			ccr, _ := rgr.Column(i)
			cols[i] = makeColumn(ccr)
		}
		result = cols
	}
	return
}

func (caller groupColumnsCaller) Docs() string {
	return `__:columns__ => _list_

Returns the columns in the group.
`
}

func makeGroup(rgr *file.RowGroupReader) (inst *flavors.Instance) {
	inst = groupFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = rgr
	return
}
