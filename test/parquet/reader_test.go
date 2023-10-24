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
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/sample.parquet")`).Eval(scope, nil)
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
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/sample.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :filepath)`,
		Expect: `"testdata/sample.parquet"`,
	}).Test(t)
}

func TestReaderVersion(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/sample.parquet")`).Eval(scope, nil)
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
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/sample.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :created-by)`,
		Expect: `"parquet-mr version 1.10.1 (build 7d648c1076647085126b685ba8288c8b8bf719ce)"`,
	}).Test(t)
}

func TestReaderRowCount(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/sample.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :row-count)`,
		Expect: "5",
	}).Test(t)
}

func TestReaderColumnCount(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/sample.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :column-count)`,
		Expect: "20",
	}).Test(t)
}

func TestReaderSchema(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/sample.parquet")`).Eval(scope, nil)
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

func TestReaderColumns(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/sample.parquet")`).Eval(scope, nil)
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
