// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"context"
	"strings"

	"github.com/apache/arrow/go/v13/arrow/memory"
	"github.com/apache/arrow/go/v13/parquet/file"
	"github.com/apache/arrow/go/v13/parquet/pqarrow"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
)

const batchSize int64 = 4096

var (
	readerFlavor *flavors.Flavor
)

func init() {
	readerFlavor = flavors.DefFlavor("parquet-reader-flavor",
		map[string]slip.Object{"filepath": nil},
		nil,
		slip.List{
			slip.List{
				slip.Symbol(":init-keywords"),
				slip.Symbol(":file"),
			},
			slip.Symbol(":gettable-instance-variables"),
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A parquet reader opens a parquet file and can be used to
access the content of that file.`),
			},
		},
	)
	readerFlavor.DefMethod(":init", "", readerInitCaller(true))
	readerFlavor.DefMethod(":close", "", readerCloseCaller(true))
	readerFlavor.DefMethod(":version", "", readerVersionCaller(true))
	readerFlavor.DefMethod(":created-by", "", readerCreatedByCaller(true))
	readerFlavor.DefMethod(":schema", "", readerSchemaCaller(true))

	readerFlavor.DefMethod(":column-count", "", readerColumnCountCaller(true))
	readerFlavor.DefMethod(":columns", "", readerColumnsCaller(true))
	readerFlavor.DefMethod(":column", "", readerColumnCaller(true))

	readerFlavor.DefMethod(":row-count", "", readerRowCountCaller(true))
	readerFlavor.DefMethod(":rows", "", readerRowsCaller(true))
	readerFlavor.DefMethod(":each-row", "", readerEachRowCaller(true))
}

type readerInitCaller bool

func (caller readerInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	var path string
	for i := 0; i < len(args); i += 2 {
		key, _ := args[i].(slip.Symbol)
		k := string(key)
		if strings.EqualFold(":file", k) {
			ss, ok := args[i+1].(slip.String)
			if !ok {
				slip.PanicType(":file", args[i+1], "string")
			}
			path = string(ss)
		}
	}
	var fr *pqarrow.FileReader
	pr, err := file.OpenParquetFile(path, true)
	if err == nil {
		fr, err = pqarrow.NewFileReader(pr, pqarrow.ArrowReadProperties{}, memory.DefaultAllocator)
	}
	if err != nil {
		panic(err)
	}
	obj.Any = fr
	obj.Let(slip.Symbol("filepath"), slip.String(path))

	return nil
}

func (caller readerInitCaller) Docs() string {
	return `__:init__ &key _file_
   _:file_ the path to a parquet file

Sets the initial values when _make-instance_ is called.
`
}

type readerCloseCaller bool

func (caller readerCloseCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
		_ = fr.ParquetReader().Close()
	}
	return nil
}

func (caller readerCloseCaller) Docs() string {
	return `__:close__ => _nil_

Close the reader.
`
}

type readerVersionCaller bool

func (caller readerVersionCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
		result = slip.String(fr.ParquetReader().MetaData().Version().String())
	}
	return
}

func (caller readerVersionCaller) Docs() string {
	return `__:version__ => _string_

Returns the version of the reader file.
`
}

type readerCreatedByCaller bool

func (caller readerCreatedByCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
		result = slip.String(fr.ParquetReader().MetaData().GetCreatedBy())
	}
	return
}

func (caller readerCreatedByCaller) Docs() string {
	return `__:created-by__ => _string_

Returns a description of the application that created of the reader file.
`
}

type readerRowCountCaller bool

func (caller readerRowCountCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
		result = slip.Fixnum(fr.ParquetReader().NumRows())
	}
	return
}

func (caller readerRowCountCaller) Docs() string {
	return `__:row-count__ => _fixnum_

Returns the number of rows in the reader file.
`
}

type readerColumnCountCaller bool

func (caller readerColumnCountCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
		sch, err := fr.Schema()
		if err != nil {
			panic(err)
		}
		result = slip.Fixnum(sch.NumFields())
	}
	return
}

func (caller readerColumnCountCaller) Docs() string {
	return `__:column-count__ => _fixnum_

Returns the number of columes int the reader file.
`
}

type readerSchemaCaller bool

func (caller readerSchemaCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
		result = makeSchemaInstance(fr.ParquetReader().MetaData().Schema.Root())
	}
	return
}

func (caller readerSchemaCaller) Docs() string {
	return `__:schema__ => _list_

Returns the schema of the reader file.
`
}

type readerColumnsCaller bool

func (caller readerColumnsCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
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
		result = columns
	}
	return
}

func (caller readerColumnsCaller) Docs() string {
	return `__:columns__ => _list_

Returns the columns of the reader file.
`
}

type readerColumnCaller bool

func (caller readerColumnCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		flavors.PanicMethodArgChoice(obj, ":column", len(args), "1")
	}
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
		schem, err := fr.Schema()
		if err != nil {
			panic(err)
		}
		if cr, _ := findColReader(fr, schem, args[0]); cr != nil {
			result = readColumn(cr)
		}
	}
	return
}

func (caller readerColumnCaller) Docs() string {
	return `__:column__ _id_ => _list_
   _id_ the name or numeric index of the column to read.

Returns the specified column of the reader file.
`
}

func (caller readerRowsCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
		rowCnt := fr.ParquetReader().NumRows()
		rows := make(slip.List, 0, rowCnt)
		var ids slip.Object
		if 1 < len(args) {
			ids = args[1]
		}
		rr := newRowReader(fr, ids, args[0])
		for {
			row := rr.next()
			if row == nil {
				break
			}
			rows = append(rows, row)
		}
		result = rows
	}
	return
}

func (caller readerRowsCaller) Docs() string {
	return `__:rows__ _format_ &optional _column-ids_ => _list_
   _format_ can be one of _:list_ for a list of column values in order,
_:assoc_ for column names as the car and the values as the cdr of each
element in an assoc list for each key value pair.
   _column-ids_ is a list of the column indexes or names for the columns to get values from.

Returns the rows of the reader file.
`
}

type readerEachRowCaller bool

func (caller readerEachRowCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	d2 := depth + 1
	fun := cl.ResolveToCaller(s, args[0], d2)
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
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
			_ = fun.Call(s, slip.List{row}, d2)
		}
	}
	return nil
}

func (caller readerEachRowCaller) Docs() string {
	return `__:each-row__ _function_ _format_ &optional _column-ids_ => _nil_
   _function_ a function or lambda to call on each row.
   _format_ can be one of _:list_ for a list of column values in order,
_:assoc_ for column names as the car and the values as the cdr of each
element in an assoc list for each key value pair.
   _column-ids_ is a list of the column indexes or names for the columns to get values from.

Calls the _function_ on each row.
`
}

func readColumn(cr *pqarrow.ColumnReader) (result slip.List) {
	for {
		ac, err := cr.NextBatch(batchSize) // *arrow.Chunked
		if err != nil {
			panic(err)
		}
		if ac.Len() == 0 {
			break
		}
		for _, chunk := range ac.Chunks() {
			cnt := chunk.Len()
			for i := 0; i < cnt; i++ {
				result = append(result, ArrayValue(chunk, i))
			}
		}
	}
	return
}
