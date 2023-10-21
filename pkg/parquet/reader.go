// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"context"
	"fmt"
	"strings"

	"github.com/apache/arrow/go/v13/arrow"
	"github.com/apache/arrow/go/v13/arrow/array"
	"github.com/apache/arrow/go/v13/arrow/memory"
	"github.com/apache/arrow/go/v13/parquet"
	"github.com/apache/arrow/go/v13/parquet/file"
	"github.com/apache/arrow/go/v13/parquet/pqarrow"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

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
	// TBD similar code to write-schema to gather nested info
	readerFlavor.DefMethod(":column-count", "", readerColumnCountCaller(true))
	readerFlavor.DefMethod(":columns", "", readerColumnsCaller(true))
	// readerFlavor.DefMethod(":column", "", readerColumnCaller(true))
	readerFlavor.DefMethod(":each-column", "", readerEachColumnCaller(true))
	readerFlavor.DefMethod(":row-count", "", readerRowCountCaller(true))
	readerFlavor.DefMethod(":rows", "", readerRowsCaller(true))
	// readerFlavor.DefMethod(":row", "", readerRowCaller(true))
	//  with arg for format :list, :assoc, :bag
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
	pr, err := file.OpenParquetFile(path, true)
	if err != nil {
		panic(err)
	}
	var fr *pqarrow.FileReader
	if fr, err = pqarrow.NewFileReader(pr, pqarrow.ArrowReadProperties{}, memory.DefaultAllocator); err != nil {
		panic(err)
	}
	obj.Any = fr
	obj.Let(slip.Symbol("filepath"), slip.String(path))

	return nil

	var schem *arrow.Schema
	if schem, err = fr.Schema(); err != nil {
		panic(err)
	}
	fmt.Printf("*** schema: %v\n", schem)
	for i := 0; i < schem.NumFields(); i++ {
		fmt.Printf("*** field %d: %s %s\n", i, schem.Field(i).Name, schem.Field(i).Type)
		var cr *pqarrow.ColumnReader
		cr, _ = fr.GetColumn(context.Background(), i)
		fmt.Printf("*** cr %d: %T %v\n", i, cr, cr)
		if cr == nil {
			break
		}
		if i == 12 {
			fmt.Printf("********** skipping %d\n", i)
			// TBD why does this panic if not skipped?
			continue
		}
		var ac *arrow.Chunked
		if ac, err = cr.NextBatch(100); err != nil {
			panic(err)
		}
		fmt.Printf("*** ac: %v\n", ac)
		for j, chunk := range ac.Chunks() {
			fmt.Printf("*** chunk %d %T len: %d\n", j, chunk, chunk.Len())
			if al, ok := chunk.(*array.List); ok {
				fmt.Printf("*** list values: %T\n", al.ListValues())
				if as, ok2 := al.ListValues().(*array.Struct); ok2 {
					fmt.Printf("*** struct size: %d %T\n", as.NumField(), as.Field(1))
					fmt.Printf("*** field values type: %T\n", as.Field(1).(*array.List).ListValues())
					af := as.Field(1).(*array.List).ListValues().(*array.Float64)
					fmt.Printf("*** floats: %d\n", af.Len())
				}
			}
		}
	}

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
		// TBD
		fmt.Printf("*** arrow file reader: %v\n", fr)
		/*
			colCnt := pr.MetaData().Schema.NumColumns()
			columns := make(slip.List, colCnt)
			rowCnt := pr.NumRows()
			rgCnt := pr.NumRowGroups()
			fmt.Printf("*** num rows: %d  num groups: %d\n", rowCnt, rgCnt)
			for i := 0; i < colCnt; i++ {
				col := make(slip.List, 0, rowCnt)
				for j := 0; j < rgCnt; j++ {
					ccr, err := pr.RowGroup(j).Column(i)
					if err != nil {
						panic(err)
					}
					col = readColumn(col, ccr)
				}
				columns[i] = col
			}
			result = columns
		*/
	}
	return
}

func (caller readerColumnsCaller) Docs() string {
	return `__:columns__ => _list_

Returns the row columns of the reader file.
`
}

type readerEachColumnCaller bool

func (caller readerEachColumnCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if fr, ok := obj.Any.(*pqarrow.FileReader); ok {
		fmt.Printf("*** arrow file reader: %v\n", fr)

		// TBD for each row group read columns

	}
	return
}

func (caller readerEachColumnCaller) Docs() string {
	return `__:each-column__ _function_ => _nil_
   _function_ the function to for each column.

Applies the _function_ to each column which is a list of values.
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

var batchSize int64 = 4096

func readColumn(col slip.List, ccr file.ColumnChunkReader) slip.List {
	fmt.Printf("*** readColumn %T\n", ccr)
	var (
		total int64
		err   error
	)
	switch tr := ccr.(type) {
	case *file.BooleanColumnChunkReader:
		values := make([]bool, batchSize)
		for {
			if total, _, err = tr.ReadBatch(batchSize, values, nil, nil); err != nil || total == 0 {
				break
			}
			for _, v := range values[:total] {
				if v {
					col = append(col, slip.True)
				} else {
					col = append(col, nil)
				}
			}
		}
	case *file.ByteArrayColumnChunkReader:
		values := make([]parquet.ByteArray, batchSize)
		for {
			if total, _, err = tr.ReadBatch(batchSize, values, nil, nil); err != nil || total == 0 {
				break
			}
			for _, v := range values[:total] {
				col = append(col, slip.String(v))
			}
		}
	case *file.FixedLenByteArrayColumnChunkReader:
		values := make([]parquet.FixedLenByteArray, batchSize)
		for {
			if total, _, err = tr.ReadBatch(batchSize, values, nil, nil); err != nil || total == 0 {
				break
			}
			for _, v := range values[:total] {
				col = append(col, slip.String(v))
			}
		}
	case *file.Int32ColumnChunkReader:
		values := make([]int32, batchSize)
		for {
			if total, _, err = tr.ReadBatch(batchSize, values, nil, nil); err != nil || total == 0 {
				break
			}
			fmt.Printf("*** total: %d\n", total, values[:total])
			for _, v := range values[:total] {
				col = append(col, slip.Fixnum(v))
			}
		}
	case *file.Int64ColumnChunkReader:
		values := make([]int64, batchSize)
		for {
			if total, _, err = tr.ReadBatch(batchSize, values, nil, nil); err != nil || total == 0 {
				break
			}
			for _, v := range values[:total] {
				col = append(col, slip.Fixnum(v))
			}
		}
	case *file.Int96ColumnChunkReader:
		values := make([]parquet.Int96, batchSize)
		for {
			if total, _, err = tr.ReadBatch(batchSize, values, nil, nil); err != nil || total == 0 {
				break
			}
			for _, v := range values[:total] {
				col = append(col, slip.Time(v.ToTime()))
			}
		}
	case *file.Float32ColumnChunkReader:
		values := make([]float32, batchSize)
		for {
			if total, _, err = tr.ReadBatch(batchSize, values, nil, nil); err != nil || total == 0 {
				break
			}
			for _, v := range values[:total] {
				col = append(col, slip.SingleFloat(v))
			}
		}
	case *file.Float64ColumnChunkReader:
		values := make([]float64, batchSize)
		for {
			if total, _, err = tr.ReadBatch(batchSize, values, nil, nil); err != nil || total == 0 {
				break
			}
			for _, v := range values[:total] {
				col = append(col, slip.DoubleFloat(v))
			}
		}
	}
	if err != nil {
		panic(err)
	}
	return col
}
