// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"io"

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
				slip.String(`A parquet schema represents a parquet schema node.`),
			},
		},
	)
	schemaFlavor.Final = true
	schemaFlavor.DefMethod(":name", "", schemaNameCaller(true))
	schemaFlavor.DefMethod(":type", "", schemaTypeCaller(true))
	schemaFlavor.DefMethod(":field-id", "", schemaFieldIDCaller(true))
	schemaFlavor.DefMethod(":repetition", "", schemaRepetitionCaller(true))
	schemaFlavor.DefMethod(":logical-type", "", schemaLogicalTypeCaller(true))
	schemaFlavor.DefMethod(":converted-type", "", schemaConvertedTypeCaller(true))
	schemaFlavor.DefMethod(":path", "", schemaPathCaller(true))
	schemaFlavor.DefMethod(":type-length", "", schemaTypeLengthCaller(true))
	schemaFlavor.DefMethod(":precision", "", schemaPrecisionCaller(true))
	schemaFlavor.DefMethod(":scale", "", schemaScaleCaller(true))
	schemaFlavor.DefMethod(":write", "", schemaWriteCaller(true))
	schemaFlavor.DefMethod(":parent", "", schemaParentCaller(true))
	schemaFlavor.DefMethod(":fields", "", schemaFieldsCaller(true))
	schemaFlavor.DefMethod(":find", "", schemaFindCaller(true))
}

type schemaNameCaller bool

func (caller schemaNameCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if node, ok := obj.Any.(schema.Node); ok {
		result = slip.String(node.Name())
	}
	return
}

func (caller schemaNameCaller) Docs() string {
	return `__:name__ => _string_

Returns the name of the schema node.
`
}

type schemaTypeCaller bool

func (caller schemaTypeCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if node, ok := obj.Any.(schema.Node); ok {
		if node.Type() == schema.Group {
			result = slip.Symbol(":group")
		} else {
			result = slip.Symbol(":primitive")
		}
	}
	return
}

func (caller schemaTypeCaller) Docs() string {
	return `__:type__ => _string_

Returns the type of the schema node which can be :group or :primitive.
`
}

type schemaFieldIDCaller bool

func (caller schemaFieldIDCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if node, ok := obj.Any.(schema.Node); ok {
		if fid := node.FieldID(); 0 <= fid {
			result = slip.Fixnum(fid)
		}
	}
	return
}

func (caller schemaFieldIDCaller) Docs() string {
	return `__:field-id__ => _fixnum_|_nil_

Returns the field ID of the schema node if there is a non negative ID.
`
}

type schemaLogicalTypeCaller bool

func (caller schemaLogicalTypeCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if node, ok := obj.Any.(schema.Node); ok {
		if lt := node.LogicalType(); lt.IsValid() && !lt.IsNone() {
			result = slip.String(lt.String())
		}
	}
	return
}

func (caller schemaLogicalTypeCaller) Docs() string {
	return `__:logical-type__ => _string_

Returns the logical type of the schema node.
`
}

type schemaConvertedTypeCaller bool

func (caller schemaConvertedTypeCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if node, ok := obj.Any.(schema.Node); ok {
		if ct := node.ConvertedType(); 0 <= ct {
			result = slip.String(ct.String())
		}
	}
	return
}

func (caller schemaConvertedTypeCaller) Docs() string {
	return `__:converted-type__ => _string_

Returns the converted type of the schema node.
`
}

type schemaPathCaller bool

func (caller schemaPathCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if node, ok := obj.Any.(schema.Node); ok {
		result = slip.String(node.Path())
	}
	return
}

func (caller schemaPathCaller) Docs() string {
	return `__:path__ => _string_

Returns the path of the schema node.
`
}

type hasLength interface {
	TypeLength() int
}

type schemaTypeLengthCaller bool

func (caller schemaTypeLengthCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if hl, ok := obj.Any.(hasLength); ok {
		if tl := hl.TypeLength(); 0 < tl {
			result = slip.Fixnum(tl)
		}
	}
	return
}

func (caller schemaTypeLengthCaller) Docs() string {
	return `__:type-length__ => _fixnum_|_nil_

Returns the type length of the schema node if the type has length of _nil_ otherwise.
`
}

type hasPrecision interface {
	Precision() int32
}

type schemaPrecisionCaller bool

func (caller schemaPrecisionCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if pn, ok := obj.Any.(*schema.PrimitiveNode); ok {
		result = slip.Fixnum(pn.DecimalMetadata().Precision)
	}
	return
}

func (caller schemaPrecisionCaller) Docs() string {
	return `__:precision__ => _fixnum_

Returns the type precision of the schema node if the type has precision of _nil_ otherwise.
`
}

type hasScale interface {
	Scale() int32
}

type schemaScaleCaller bool

func (caller schemaScaleCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if pn, ok := obj.Any.(*schema.PrimitiveNode); ok {
		result = slip.Fixnum(pn.DecimalMetadata().Scale)
	}
	return
}

func (caller schemaScaleCaller) Docs() string {
	return `__:scale__ => _fixnum_|_nil_

Returns the type scale of the schema node if the type has scale of _nil_ otherwise.
`
}

type schemaRepetitionCaller bool

func (caller schemaRepetitionCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if node, ok := obj.Any.(schema.Node); ok {
		// Internal constants not visible so just use the integer values.
		switch node.RepetitionType() {
		case 0:
			result = slip.Symbol(":required")
		case 1:
			result = slip.Symbol(":optional")
		case 2:
			result = slip.Symbol(":repeated")
		}
	}
	return
}

func (caller schemaRepetitionCaller) Docs() string {
	return `__:repetition__ => _keyword_

Returns the repetition type of the schema node which can be :required, :optional, or :repeated.
`
}

type schemaWriteCaller bool

func (caller schemaWriteCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if len(args) != 1 {
		flavors.PanicMethodArgChoice(obj, "destination", len(args), "1")
	}
	if node, ok := obj.Any.(schema.Node); ok {
		sa := schemaAppender{pad: 2}
		node.Visit(&sa)
		switch ta := args[0].(type) {
		case nil:
			result = slip.String(sa.buf)
		case io.Writer:
			if _, err := ta.Write(sa.buf); err != nil {
				panic(err)
			}
		default:
			if ta == slip.True {
				if _, err := s.Get(slip.Symbol("*standard-output*")).(io.Writer).Write(sa.buf); err != nil {
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

type schemaParentCaller bool

func (caller schemaParentCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if node, ok := obj.Any.(schema.Node); ok {
		if parent := node.Parent(); parent != nil {
			result = makeSchemaInstance(parent)
		}
	}
	return
}

func (caller schemaParentCaller) Docs() string {
	return `__:parent__ => _instance_

Returns the parent of the schema node if it has one.
`
}

type schemaFieldsCaller bool

func (caller schemaFieldsCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if node, ok := obj.Any.(schema.Node); ok && node.Type() == schema.Group {
		g := node.(*schema.GroupNode)
		cnt := g.NumFields()
		fields := make(slip.List, cnt)
		for i := 0; i < cnt; i++ {
			fields[i] = makeSchemaInstance(g.Field(i))
		}
		result = fields
	}
	return
}

func (caller schemaFieldsCaller) Docs() string {
	return `__:fields__ => _list_

Returns the fields of the schema node if it is a group node.
`
}

type schemaFindCaller bool

func (caller schemaFindCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if node, ok := obj.Any.(schema.Node); ok {
		path, ok := args[0].(slip.List)
		if !ok {
			path = args
		}
		for _, x := range path {
			var gn *schema.GroupNode
			if gn, ok = node.(*schema.GroupNode); !ok {
				return
			}
			switch tx := x.(type) {
			case slip.Fixnum:
				if tx < 0 || gn.NumFields() <= int(tx) {
					return
				}
				node = gn.Field(int(tx))
			case slip.String:
				i := gn.FieldIndexByName(string(tx))
				if i < 0 {
					return
				}
				node = gn.Field(i)
			case slip.Symbol:
				i := gn.FieldIndexByName(string(tx))
				if i < 0 {
					return
				}
				node = gn.Field(i)
			default:
				slip.PanicType("path member", tx, "fixnum", "string", "symbol")
			}
		}
		result = makeSchemaInstance(node)
	}
	return
}

func (caller schemaFindCaller) Docs() string {
	return `__:find__ &rest _path_ => _instance_|_nil_
   _path_ either an _fixnum_, _string_, or a list of _fixnum_ and _strings_ that describe a
path to a child in the schema.

Finds a field schema in the schema based on the _path_ where a _fixnum_ indicates the index
of the child field and a _string_ inicates the name of a child field.
`
}

func makeSchemaInstance(node schema.Node) (inst *flavors.Instance) {
	inst = schemaFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = node
	return
}
