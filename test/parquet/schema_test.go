// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"fmt"
	"strings"
	"testing"

	pq "github.com/apache/arrow/go/v13/parquet"
	"github.com/apache/arrow/go/v13/parquet/schema"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
	"github.com/ohler55/slip/pkg/parquet"
	"github.com/ohler55/slip/sliptest"
)

type badWriter int

func (w badWriter) Write([]byte) (int, error) {
	return 0, fmt.Errorf("failed")
}

func TestSchemaDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method parquet-schema-flavor :name out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":name"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :type out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":type"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :field-id out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":field-id"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :repetition out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":repetition"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :logical-type out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":logical-type"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :converted-type out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":converted-type"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :path out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":path"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :type-length out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":type-length"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :precision out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":precision"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :scale out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":scale"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :write out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":write"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :parent out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":parent"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :fields out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":fields"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :find out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":find"))
}

func TestSchemaFindName(t *testing.T) {
	scope := setupSchemaTest(t, "nested-maps.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :name)`,
		Expect: `"spark_schema"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find 0) :name)`,
		Expect: `"a"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find '(a key_value key)) :name)`,
		Expect: `"key"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find "a" 0 0) :name)`,
		Expect: `"key"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :find -1)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :find -1)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :find 1 1)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :find "nothing")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :find 'nothing)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send schema :find t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestSchemaType(t *testing.T) {
	scope := setupSchemaTest(t, "nested-maps.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :type)`,
		Expect: ":group",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find 1) :type)`,
		Expect: ":primitive",
	}).Test(t)
}

func TestSchemaFieldID(t *testing.T) {
	scope := setupSchemaTest(t, "primitive.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :field-id)`,
		Expect: "nil",
	}).Test(t)

	pn := schema.NewFixedLenByteArrayNode("dummy", 0, 5, 7)
	inst := parquet.SchemaFlavor().MakeInstance().(*flavors.Instance)
	inst.Any = pn
	scope.Let("schema", inst)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :field-id)`,
		Expect: "7",
	}).Test(t)
}

func TestSchemaLogicalType(t *testing.T) {
	scope := setupSchemaTest(t, "byte-array-decimal.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :logical-type)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find 0) :logical-type)`,
		Expect: `"Decimal(precision=4, scale=2)"`,
	}).Test(t)
}

func TestSchemaConvertedType(t *testing.T) {
	scope := setupSchemaTest(t, "byte-array-decimal.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :converted-type)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find 0) :converted-type)`,
		Expect: `"DECIMAL"`,
	}).Test(t)
}

func TestSchemaPath(t *testing.T) {
	scope := setupSchemaTest(t, "nested-maps.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :path)`,
		Expect: `""`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find '(0 0 1 0 0)) :path)`,
		Expect: `"a.key_value.value.key_value.key"`,
	}).Test(t)
}

func TestSchemaTypeLength(t *testing.T) {
	scope := setupSchemaTest(t, "primitive.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :type-length)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find 9) :type-length)`,
		Expect: "nil",
	}).Test(t)

	pn := schema.NewFixedLenByteArrayNode("dummy", 0, 5, 7)
	inst := parquet.SchemaFlavor().MakeInstance().(*flavors.Instance)
	inst.Any = pn
	scope.Let("schema", inst)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :type-length)`,
		Expect: "5",
	}).Test(t)
}

func TestSchemaPrecision(t *testing.T) {
	scope := setupSchemaTest(t, "byte-array-decimal.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :precision)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find 0) :precision)`,
		Expect: "4",
	}).Test(t)

	type Dummy struct {
		Dec int64 `parquet:"logical=decimal, logical.precision=9, logical.scale=3"`
	}
	sch, err := schema.NewSchemaFromStruct(Dummy{})
	tt.Nil(t, err)
	node := sch.Root().Field(0)
	inst := parquet.SchemaFlavor().MakeInstance().(*flavors.Instance)
	inst.Any = node
	scope.Let("schema", inst)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :precision)`,
		Expect: "9",
	}).Test(t)
}

func TestSchemaScale(t *testing.T) {
	scope := setupSchemaTest(t, "byte-array-decimal.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :scale)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find 0) :scale)`,
		Expect: "2",
	}).Test(t)
}

func TestSchemaRepetition(t *testing.T) {
	scope := setupSchemaTest(t, "nested-maps.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :repetition)`,
		Expect: ":required",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find 0) :repetition)`,
		Expect: ":optional",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find 'a 'key_value) :repetition)`,
		Expect: ":repeated",
	}).Test(t)
}

func TestSchemaParent(t *testing.T) {
	scope := setupSchemaTest(t, "nested-maps.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :parent)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send (send schema :find 0) :parent) :name)`,
		Expect: `"spark_schema"`,
	}).Test(t)
}

func TestSchemaFields(t *testing.T) {
	scope := setupSchemaTest(t, "nested-maps.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :fields)`,
		Validate: func(t *testing.T, v slip.Object) {
			list := v.(slip.List)
			tt.Equal(t, 3, len(list))
			inst := list[0].(*flavors.Instance)
			tt.Equal(t, "parquet-schema-flavor", inst.Flavor.Name())
		},
	}).Test(t)
}

func TestSchemaWrite(t *testing.T) {
	var out strings.Builder
	scope := setupSchemaTest(t, "nested-maps.parquet")
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	scope.Let("out", &slip.OutputStream{Writer: &out})

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :write out)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t,
		`required group spark_schema {
  optional group a (Map) {
    repeated group key_value {
      required byte_array key (String);
      optional group value (Map) {
        repeated group key_value {
          required int32 key;
          required boolean value;
        }
      }
    }
  }
  required int32 b;
  required double c;
}
`, out.String())
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find 0 0 0) :write nil)`,
		Expect: `"required byte_array key (String);
"`,
	}).Test(t)
	out.Reset()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send (send schema :find 0 0 0) :write t)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `required byte_array key (String);
`, out.String())

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send schema :write 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send schema :write)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)

	scope.Let("*standard-output*", &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send schema :write t)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
	scope.Let("out", &slip.OutputStream{Writer: badWriter(0)})
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send schema :write out)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

// There doesn't seem to be a way to create a schema.Node with the right
// characteristics so this allows full coverage on the write tests.
type fakeNode struct {
	schema.PrimitiveNode
	name      string
	typ       schema.NodeType
	physical  pq.Type
	logical   schema.LogicalType
	converted schema.ConvertedType
	decMeta   schema.DecimalMetadata
}

func (n *fakeNode) Name() string {
	return n.name
}

func (n *fakeNode) Type() schema.NodeType {
	return n.typ
}

func (n *fakeNode) ConvertedType() schema.ConvertedType {
	return n.converted
}

func (n *fakeNode) LogicalType() schema.LogicalType {
	return n.logical
}

func (n *fakeNode) Visit(v schema.Visitor) {
	v.VisitPre(n)
	v.VisitPost(n)
}

func (n *fakeNode) PhysicalType() pq.Type {
	return n.physical
}

func (n *fakeNode) DecimalMetadata() schema.DecimalMetadata {
	return n.decMeta
}

func TestSchemaWrite2(t *testing.T) {
	type In struct {
		Date int32 `parquet:"logical=date, fieldid=3"`
		Con  int64 `parquet:"converted=decimal, precision=9, scale=3"`
	}
	type Dummy struct {
		Dec int64 `parquet:"logical=decimal, logical.precision=9, logical.scale=3, fieldid=1"`
		In  In    `parquet:"fieldid=2"`
	}
	sch, err := schema.NewSchemaFromStruct(Dummy{})
	tt.Nil(t, err)
	node := sch.Root()
	inst := parquet.SchemaFlavor().MakeInstance().(*flavors.Instance)
	inst.Any = node
	scope := slip.NewScope()
	scope.Let("schema", inst)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :write nil)`,
		Expect: `"repeated group Dummy {
  required int64 field_id=1 Dec (Decimal(precision=9, scale=3));
  required group field_id=2 In {
    required int32 field_id=3 Date (Date);
    required int64 Con (Decimal(precision=9, scale=3));
  }
}
"`,
	}).Test(t)

	inst.Any = &fakeNode{
		name:      "fake",
		typ:       schema.Primitive,
		physical:  5, // double but private
		logical:   schema.NoLogicalType{},
		converted: schema.ConvertedTypes.Decimal,
		decMeta:   schema.DecimalMetadata{IsSet: true, Scale: 3, Precision: 5},
	}
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :write nil)`,
		Expect: `"required double field_id=0 fake (Decimal(precision=5, scale=3));
"`,
	}).Test(t)

	inst.Any = &fakeNode{
		name:      "fake",
		typ:       schema.Primitive,
		physical:  2, // int64 but private
		logical:   schema.NoLogicalType{},
		converted: schema.ConvertedTypes.Int64,
	}
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :write nil)`,
		Expect: `"required int64 field_id=0 fake (Int_64);
"`,
	}).Test(t)

	inst.Any = &fakeNode{
		name: "fake",
		typ:  schema.Group,
		//physical:  2, //  but private
		logical:   schema.NoLogicalType{},
		converted: schema.ConvertedTypes.List,
	}
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send schema :write nil)`,
		Expect: `"required group field_id=0 fake (List) {
}
"`,
	}).Test(t)

}

func setupSchemaTest(t *testing.T, filename string) *slip.Scope {
	scope := slip.NewScope()
	pr := slip.ReadString(
		fmt.Sprintf(`(make-instance 'parquet-reader-flavor :file "testdata/%s")`, filename)).Eval(scope, nil)
	scope.Let("reader", pr)
	schema := slip.ReadString(`(send reader :schema)`).Eval(scope, nil)
	scope.Let("schema", schema)

	return scope
}
