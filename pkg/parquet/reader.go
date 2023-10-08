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
