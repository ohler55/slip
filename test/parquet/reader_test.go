// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReaderBasic(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `reader`,
		Expect: "/^#<parquet-reader-flavor [0-9a-f]+>$/",
	}).Test(t)
}

func TestReaderFilepath(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :filepath)`,
		Expect: `"testdata/primitive.parquet"`,
	}).Test(t)
}

func TestReaderVersion(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :version)`,
		Expect: `"v1.0"`,
	}).Test(t)
}

func TestReaderCreatedBy(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :created-by)`,
		Expect: `"impala version 1.3.0-INTERNAL (build 8a48ddb1eff84592b3fc06bc6f51ec120e1fffc9)"`,
	}).Test(t)
}

func TestReaderRowCount(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :row-count)`,
		Expect: "8",
	}).Test(t)
}

func TestReaderColumnCount(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :column-count)`,
		Expect: "11",
	}).Test(t)
}

func TestReaderSchema(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :schema)`,
		Expect: "/^#<parquet-schema-flavor [0-9a-f]+>$/",
	}).Test(t)
}

func appendValue(b []byte, value slip.Object) []byte {
	switch tv := value.(type) {
	case slip.String:
		b = append(b, string(tv)...)
	default:
		b = slip.Append(b, tv)
	}
	return b
}

func TestReaderDocs(t *testing.T) {
	scope := slip.NewScope()
	var out strings.Builder
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :init out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":init"))

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :close out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":close"))

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :version out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":version"))

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :created-by out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":created-by"))

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :row-count out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":row-count"))

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :column-count out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":column-count"))

	_ = slip.ReadString(`(describe-method parquet-reader-flavor :schema out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":schema"))
}

func TestReaderBadInitFile(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() {
		_ = slip.ReadString(`(make-instance 'parquet-reader-flavor :file "no-file")`).Eval(scope, nil)
	})
	tt.Panic(t, func() {
		_ = slip.ReadString(`(make-instance 'parquet-reader-flavor :file t)`).Eval(scope, nil)
	})
}

func TestReaderColumnsPrimitive(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :columns)`,
		Validate: func(t *testing.T, v slip.Object) {
			vlist := v.(slip.List)
			for i, x := range []string{
				"(4 5 6 7 2 3 0 1)",
				"(t nil t nil t nil t nil)",
				"(0 1 0 1 0 1 0 1)",
				"(0 1 0 1 0 1 0 1)",
				"(0 1 0 1 0 1 0 1)",
				"(0 10 0 10 0 10 0 10)",
				"(0 1.1 0 1.1 0 1.1 0 1.1)",
				"(0 10.1 0 10.1 0 10.1 0 10.1)",
				`("03/01/09" "03/01/09" "04/01/09" "04/01/09" "02/01/09" "02/01/09" "01/01/09" "01/01/09")`,
				`("0" "1" "0" "1" "0" "1" "0" "1")`,
			} {
				tt.Equal(t, x, slip.ObjectString(vlist[i]))
			}
			for i, x := range []string{
				"@2009-03-01T00:00:00Z",
				"@2009-03-01T00:01:00Z",
				"@2009-04-01T00:00:00Z",
				"@2009-04-01T00:01:00Z",
				"@2009-02-01T00:00:00Z",
				"@2009-02-01T00:01:00Z",
				"@2009-01-01T00:00:00Z",
				"@2009-01-01T00:01:00Z",
			} {
				tt.Equal(t, x, slip.ObjectString(vlist[10].(slip.List)[i]))
			}
		},
	}).Test(t)
}

func TestReaderColumnsNestedList(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(
		`(make-instance 'parquet-reader-flavor :file "testdata/nested-list.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :columns)`,
		Validate: func(t *testing.T, v slip.Object) {
			vlist := v.(slip.List)
			for i, x := range []string{
				`((("a" "b") ("c")) (nil ("d")))`,
				`((("a" "b") ("c" "d")) (nil ("e")))`,
				`((("a" "b") ("c" "d") ("e")) (nil ("f")))`,
			} {
				tt.Equal(t, x, slip.ObjectString(vlist[0].(slip.List)[i]))
			}
			tt.Equal(t, "(1 1 1)", slip.ObjectString(vlist[1]))
		},
	}).Test(t)
}

func TestReaderColumnsNestedMap(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(
		`(make-instance 'parquet-reader-flavor :file "testdata/nested-maps.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :columns)`,
		Validate: func(t *testing.T, v slip.Object) {
			fmt.Printf("*** validate %T %s\n", v, v)
		},
	}).Test(t)
}
