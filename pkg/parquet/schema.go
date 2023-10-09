// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	// pi "github.com/apache/arrow/go/v13/parquet/internal/gen-go/parquet"

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
	// TBD other methods
}

type schemaNameCaller bool

func (caller schemaNameCaller) Call(s *slip.Scope, args slip.List, _ int) (result slip.Object) {
	obj := s.Get("self").(*flavors.Instance)
	if se, ok := obj.Any.(*schemaElement); ok {
		result = slip.String(se.name)
	}
	return
}

func (caller schemaNameCaller) Docs() string {
	return `__:name__ => _string_

Returns the name of the schema element.
`
}

type schemaElement struct {
	name string
	typ  string
	/*
		TypeLength     int32 // SchemaElement.GetTypeLength()
		RepetitionType *FieldRepetitionType `thrift:"repetition_type,3" db:"repetition_type" json:"repetition_type,omitempty"`
		NumChildren    *int32               `thrift:"num_children,5" db:"num_children" json:"num_children,omitempty"`
		ConvertedType  *ConvertedType       `thrift:"converted_type,6" db:"converted_type" json:"converted_type,omitempty"`
		Scale          *int32               `thrift:"scale,7" db:"scale" json:"scale,omitempty"`
		Precision      *int32               `thrift:"precision,8" db:"precision" json:"precision,omitempty"`
		FieldID        *int32               `thrift:"field_id,9" db:"field_id" json:"field_id,omitempty"`
		LogicalType    *LogicalType         `thrift:"logicalType,10" db:"logicalType" json:"logicalType,omitempty"`
	*/
}

func makeSchemaElement(se *schemaElement) (inst *flavors.Instance) {
	inst = schemaFlavor.MakeInstance().(*flavors.Instance)
	inst.Any = se
	return
}
