// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"context"
	"encoding/json"
	"fmt"
	"strings"

	"github.com/apache/arrow/go/v13/arrow"
	"github.com/apache/arrow/go/v13/arrow/array"
	"github.com/apache/arrow/go/v13/arrow/memory"
	"github.com/apache/arrow/go/v13/parquet/file"
	"github.com/apache/arrow/go/v13/parquet/pqarrow"
	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/slip"
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
	//  with arg for format :list, :assoc, :bag
	readerFlavor.DefMethod(":each-row", "", readerEachRowCaller(true))
	// TBD option to just include specific columns as well as format
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
		var cr *pqarrow.ColumnReader
		switch id := args[0].(type) {
		case slip.Fixnum:
			if 0 <= id && int(id) < schem.NumFields() {
				cr, _ = fr.GetColumn(context.Background(), int(id))
			}
		case slip.String:
			if fa := schem.FieldIndices(string(id)); 0 < len(fa) {
				cr, _ = fr.GetColumn(context.Background(), fa[0])
			}
		case slip.Symbol:
			if fa := schem.FieldIndices(string(id)); 0 < len(fa) {
				cr, _ = fr.GetColumn(context.Background(), fa[0])
			}
		default:
			slip.PanicType("id", id, "fixnum", "string", "symbol")
		}
		if cr != nil {
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

type readerRowsCaller bool

func (caller readerRowsCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
		rowCnt := fr.ParquetReader().NumRows()
		rows := make(slip.List, rowCnt)

		fmt.Printf("*** row count: %d\n", rowCnt)
		// TBD for each row group read rows

		result = rows
	}
	return
}

func (caller readerRowsCaller) Docs() string {
	return `__:rows__ => _list_

Returns the row columns of the reader file.
`
}

type readerEachRowCaller bool

func (caller readerEachRowCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
		rowCnt := fr.ParquetReader().NumRows()

		fmt.Printf("*** row count: %d\n", rowCnt)
		// TBD for each row group read columns

		// TBD option for row as list, assoc, or bag

	}
	return nil
}

func (caller readerEachRowCaller) Docs() string {
	return `__:each-row__ _function_ => _nil_
   _function_ the function to for each rwo.

Applies the _function_ to each row which is a list of values.
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
			result = arrayToLisp(result, chunk)
		}
	}
	return
}

func arrayToLisp(col slip.List, aa arrow.Array) slip.List {
	cnt := aa.Len()
	switch tc := aa.(type) {
	case *array.Null: // type having no physical storage
		for i := 0; i < cnt; i++ {
			col = append(col, slip.SimpleObject(tc.Value(i)))
		}
	case *array.Boolean: // is a 1 bit, LSB bit-packed ordering
		for i := 0; i < cnt; i++ {
			if tc.Value(i) {
				col = append(col, slip.True)
			} else {
				col = append(col, nil)
			}
		}
	case *array.Uint8: // a Unsigned 8-bit little-endian integer
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Fixnum(tc.Value(i)))
		}
	case *array.Int8: // a Signed 8-bit little-endian integer
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Fixnum(tc.Value(i)))
		}
	case *array.Uint16: // a Unsigned 16-bit little-endian integer
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Fixnum(tc.Value(i)))
		}
	case *array.Int16: // a Signed 16-bit little-endian integer
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Fixnum(tc.Value(i)))
		}
	case *array.Uint32: // a Unsigned 32-bit little-endian integer
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Fixnum(tc.Value(i)))
		}
	case *array.Int32: // a Signed 32-bit little-endian integer
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Fixnum(tc.Value(i)))
		}
	case *array.Uint64: // a Unsigned 64-bit little-endian integer
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Fixnum(tc.Value(i)))
		}
	case *array.Int64: // a Signed 64-bit little-endian integer
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Fixnum(tc.Value(i)))
		}
	case *array.Float16: // an 2-byte floating point value
		for i := 0; i < cnt; i++ {
			col = append(col, slip.SingleFloat(tc.Value(i).Float32()))
		}
	case *array.Float32: // an 4-byte floating point value
		for i := 0; i < cnt; i++ {
			col = append(col, slip.SingleFloat(tc.Value(i)))
		}
	case *array.Float64: // an 8-byte floating point value
		for i := 0; i < cnt; i++ {
			col = append(col, slip.DoubleFloat(tc.Value(i)))
		}
	case *array.String: // a UTF8 variable-length string
		for i := 0; i < cnt; i++ {
			col = append(col, slip.String(tc.Value(i)))
		}
	case *array.Binary: // a Variable-length byte type (no guarantee of UTF8-ness)
		for i := 0; i < cnt; i++ {
			col = append(col, slip.String(tc.Value(i)))
		}
	case *array.FixedSizeBinary: // a binary where each value occupies the same number of bytes
		for i := 0; i < cnt; i++ {
			col = append(col, slip.String(tc.Value(i)))
		}
	case *array.Date32: // int32 days since the UNIX epoch
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Time(tc.Value(i).ToTime()))
		}
	case *array.Date64: // int64 milliseconds since the UNIX epoch
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Time(tc.Value(i).ToTime()))
		}
	case *array.Timestamp: // an exact timestamp encoded with int64 since UNIX epoch Default unit millisecond
		tu := arrow.Millisecond
		if at, ok := tc.DataType().(*arrow.TimestampType); ok {
			tu = at.Unit
		}
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Time(tc.Value(i).ToTime(tu)))
		}
	case *array.Time32: // a signed 32-bit integer, representing either seconds or milliseconds since midnight
		tu := arrow.Millisecond
		if at, ok := tc.DataType().(*arrow.Time32Type); ok {
			tu = at.Unit
		}
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Time(tc.Value(i).ToTime(tu)))
		}
	case *array.Time64: // a signed 64-bit integer, representing either microseconds or nanoseconds since midnight
		tu := arrow.Nanosecond
		if at, ok := tc.DataType().(*arrow.Time64Type); ok {
			tu = at.Unit
		}
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Time(tc.Value(i).ToTime(tu)))
		}
	case *array.Struct: // struct of logical types
		for i := 0; i < cnt; i++ {
			col = append(col, forMarshalToLisp(tc.GetOneForMarshal(i)))
		}
	case *array.Map: // a repeated struct logical type
		items := arrayToLisp(nil, tc.Items())
		keys := arrayToLisp(nil, tc.Keys())
		for i, k := range keys {
			col = append(col, slip.List{k, slip.Tail{Value: items[i]}})
		}
	case *array.Duration: // Measure of elapsed time in either seconds, milliseconds, microseconds or nanoseconds
		for i := 0; i < cnt; i++ {
			col = append(col, slip.Fixnum(tc.Value(i)))
		}
	default:
		// Includes array.Struct and array.List since there does not appear to
		// be any way to get the data for each item so the horribly
		// inefficient approach is taken of getting the JSON encoded values
		// and parsing.
		for i := 0; i < cnt; i++ {
			col = append(col, forMarshalToLisp(tc.GetOneForMarshal(i)))
		}
	}
	return col
}

func forMarshalToLisp(m any) slip.Object {
	if raw, ok := m.(json.RawMessage); ok {
		m = oj.MustParse([]byte(raw))
	}
	return slip.SimpleObject(m)
}
