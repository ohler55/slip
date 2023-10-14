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
	schemaFlavor.DefMethod(":logical-type", "", schemaLogicalTypeCaller(true))
	schemaFlavor.DefMethod(":converted-type", "", schemaConvertedTypeCaller(true))
	schemaFlavor.DefMethod(":path", "", schemaPathCaller(true))
	schemaFlavor.DefMethod(":type-length", "", schemaTypeLengthCaller(true))
	schemaFlavor.DefMethod(":repetition", "", schemaRepetitionCaller(true))
	schemaFlavor.DefMethod(":max-definitions", "", schemaMaxDefinitionsCaller(true))
	schemaFlavor.DefMethod(":write", "", schemaWriteCaller(true))
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

type schemaLogicalTypeCaller bool

func (caller schemaLogicalTypeCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if sc, ok := obj.Any.(*schema.Column); ok {
		if lt := sc.LogicalType(); lt.IsValid() && !lt.IsNone() {
			result = slip.String(lt.String())
		}
	}
	return
}

func (caller schemaLogicalTypeCaller) Docs() string {
	return `__:logical-type__ => _string_

Returns the logical type of the schema element.
`
}

type schemaConvertedTypeCaller bool

func (caller schemaConvertedTypeCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if sc, ok := obj.Any.(*schema.Column); ok {
		if ct := sc.ConvertedType(); 0 <= ct {
			result = slip.String(ct.String())
		}
	}
	return
}

func (caller schemaConvertedTypeCaller) Docs() string {
	return `__:converted-type__ => _string_

Returns the converted type of the schema element.
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

type schemaTypeLengthCaller bool

func (caller schemaTypeLengthCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if sc, ok := obj.Any.(*schema.Column); ok {
		if tl := sc.TypeLength(); 0 < tl {
			result = slip.Fixnum(tl)
		}
	}
	return
}

func (caller schemaTypeLengthCaller) Docs() string {
	return `__:type-length__ => _fixnum_

Returns the type length of the schema element.
`
}

type schemaRepetitionCaller bool

func (caller schemaRepetitionCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if sc, ok := obj.Any.(*schema.Column); ok {
		switch sc.MaxRepetitionLevel() {
		case 0:
			result = slip.String("required")
		case 1:
			result = slip.String("optional")
		case 2:
			result = slip.String("repeated")
		}
	}
	return
}

func (caller schemaRepetitionCaller) Docs() string {
	return `__:repetition__ => _string_

Returns the repetition of the schema element.
`
}

type schemaMaxDefinitionsCaller bool

func (caller schemaMaxDefinitionsCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if sc, ok := obj.Any.(*schema.Column); ok {
		if md := sc.MaxDefinitionLevel(); 0 < md {
			result = slip.Fixnum(md)
		}
	}
	return
}

func (caller schemaMaxDefinitionsCaller) Docs() string {
	return `__:max-definitions__ => _fixnum_

Returns the maximum definitions of the schema element.
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
