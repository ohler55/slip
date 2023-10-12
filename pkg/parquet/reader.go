// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"strings"

	"github.com/apache/arrow/go/v13/parquet/file"
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
	readerFlavor.DefMethod(":row-count", "", readerRowCountCaller(true))
	readerFlavor.DefMethod(":column-count", "", readerColumnCountCaller(true))
	readerFlavor.DefMethod(":group-count", "", readerGroupCountCaller(true))
	readerFlavor.DefMethod(":schema", "", readerSchemaCaller(true))
	// TBD other methods
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
	obj.Any = pr
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
	if pr, ok := obj.Any.(*file.Reader); ok {
		_ = pr.Close()
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
	if pr, ok := obj.Any.(*file.Reader); ok {
		result = slip.String(pr.MetaData().Version().String())
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
	if pr, ok := obj.Any.(*file.Reader); ok {
		result = slip.String(pr.MetaData().GetCreatedBy())
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
	if pr, ok := obj.Any.(*file.Reader); ok {
		result = slip.Fixnum(pr.NumRows())
	}
	return
}

func (caller readerRowCountCaller) Docs() string {
	return `__:row-count__ => _fixnum_

Returns the number of rows in the reader file.
`
}

type readerGroupCountCaller bool

func (caller readerGroupCountCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if pr, ok := obj.Any.(*file.Reader); ok {
		result = slip.Fixnum(pr.NumRowGroups())
	}
	return
}

func (caller readerGroupCountCaller) Docs() string {
	return `__:group-count__ => _fixnum_

Returns the number of row groups in the reader file.
`
}

type readerColumnCountCaller bool

func (caller readerColumnCountCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if pr, ok := obj.Any.(*file.Reader); ok {
		result = slip.Fixnum(len(pr.MetaData().FileMetaData.ColumnOrders))
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
	if pr, ok := obj.Any.(*file.Reader); ok {
		schema := pr.MetaData().Schema
		colCnt := schema.NumColumns()
		cols := make(slip.List, colCnt)
		for i := 0; i < colCnt; i++ {
			cols[i] = makeSchemaElement(schema.Column(i))
		}
		result = cols
	}
	return
}

func (caller readerSchemaCaller) Docs() string {
	return `__:schema__ => _list_

Returns the schema of the reader file.
`
}
