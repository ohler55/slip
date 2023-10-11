// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"fmt"
	"io"
	"strings"

	"github.com/apache/arrow/go/v13/parquet/schema"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var schemaFlavor *flavors.Flavor

func init() {
	schemaFlavor = flavors.DefFlavor("parquet-schema-flavor",
		map[string]slip.Object{},
		nil,
		slip.List{
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A parquet schema represents a parquet schema element.`),
			},
		},
	)
	schemaFlavor.Final = true
	schemaFlavor.DefMethod(":name", "", schemaNameCaller(true))
	schemaFlavor.DefMethod(":type", "", schemaTypeCaller(true))
	schemaFlavor.DefMethod(":path", "", schemaPathCaller(true))

	schemaFlavor.DefMethod(":write", "", schemaWriteCaller(true))

	// TBD other methods
}

type schemaNameCaller bool

func (caller schemaNameCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if sc, ok := obj.Any.(*schema.Column); ok {
		result = slip.String(sc.Name())
	}
	return
}

func (caller schemaNameCaller) Docs() string {
	return `__:name__ => _string_

Returns the name of the schema element.
`
}

type schemaTypeCaller bool

func (caller schemaTypeCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if sc, ok := obj.Any.(*schema.Column); ok {
		result = slip.String(sc.PhysicalType().String())
	}
	return
}

func (caller schemaTypeCaller) Docs() string {
	return `__:type__ => _string_

Returns the type of the schema element.
`
}

type schemaPathCaller bool

func (caller schemaPathCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if sc, ok := obj.Any.(*schema.Column); ok {
		result = slip.String(sc.Path())
	}
	return
}

func (caller schemaPathCaller) Docs() string {
	return `__:path__ => _string_

Returns the path of the schema element.
`
}

type schemaWriteCaller bool

func (caller schemaWriteCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		flavors.PanicMethodArgChoice(obj, "destination", len(args), "1")
	}
	if sc, ok := obj.Any.(*schema.Column); ok {
		var (
			line string
			rep  string
		)
		pt := sc.PhysicalType().String()
		ct := sc.ConvertedType().String()
		if ct == "NONE" {
			ct = ""
		}
		switch sc.MaxRepetitionLevel() {
		case 0:
			rep = "optional"
		case 1:
			rep = "required"
		default:
			rep = "repeated"
		}
		if 0 < len(ct) {
			line = fmt.Sprintf("  %s %s %s (%s);\n", rep, strings.ToLower(pt), sc.Name(), ct)
		} else {
			line = fmt.Sprintf("  %s %s %s;\n", rep, strings.ToLower(pt), sc.Name())
		}
		switch ta := args[0].(type) {
		case nil:
			result = slip.String(line)
		case io.Writer:
			if _, err := ta.Write([]byte(line)); err != nil {
				panic(err)
			}
		default:
			if ta == slip.True {
				if _, err := s.Get(slip.Symbol("*standard-output*")).(io.Writer).Write([]byte(line)); err != nil {
					panic(err)
				}
			} else {
				slip.PanicType("destination", ta, "nil", "t", "output-stream")
			}
		}
	}
	return
}

func (caller schemaWriteCaller) Docs() string {
	return `__:write__ _destination_ => _string_|_nil_
   _destination_ either an _output-stream_, _t_, or _nil_. If an _output-stream_ then
write to it. If _t_ then write to _*standard-output*_. If _nil_ then return a string.

Writes a line as if it were part of a schema file.
`
}

func makeSchemaElement(sc *schema.Column) (inst *flavors.Instance) {
	inst = schemaFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = sc
	return
}
