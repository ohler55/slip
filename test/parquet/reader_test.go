// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"fmt"
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
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :filepath)`,
		Expect: `"testdata/sample.parquet"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :version)`,
		Expect: `"v1.0"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :created-by)`,
		Expect: `"parquet-mr version 1.10.1 (build 7d648c1076647085126b685ba8288c8b8bf719ce)"`,
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :row-count)`,
		Expect: "5",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :group-count)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :column-count)`,
		Expect: "45",
	}).Test(t)
	tf := sliptest.Function{
		Scope:  scope,
		Source: `(send reader :schema)`,
		Expect: `/:name . "event_date"/`,
	}
	tf.Test(t)
	var b []byte
	for i, col := range tf.Result.(slip.List) {
		for _, a := range col.(slip.List) {
			b = a.Append(b)
			b = append(b, '\n')
		}
		b = append(b, '\n')
		if 0 < i {
			break
		}
	}
	tt.Equal(t, `(:name . "event_date")
(:path . "event_date")
(:logical-type . "Date")
(:converted-type . "DATE")
(:physical-type . "INT32")
(:max-repetition . 0)
(:max-definitions . 1)

(:name . "request_id")
(:path . "request_id")
(:logical-type . "String")
(:converted-type . "UTF8")
(:physical-type . "BYTE_ARRAY")
(:max-repetition . 0)
(:max-definitions . 1)

`, string(b))

	schema := slip.ReadString(`(send reader :foo)`).Eval(scope, nil).(slip.List)
	b = b[:0]
	for _, element := range schema {
		scope.Let(slip.Symbol("element"), element)
		b = slip.Append(b, slip.ReadString(`(send element :name)`).Eval(scope, nil))
		b = append(b, '\n')
	}
	fmt.Printf("*** %s\n", b)

	b = b[:0]
	for _, element := range schema {
		scope.Let(slip.Symbol("element"), element)
		b = append(b, slip.ReadString(`(send element :write nil)`).Eval(scope, nil).(slip.String)...)
	}
	fmt.Printf("*** %s\n", b)

	b = b[:0]
	for _, element := range schema {
		scope.Let(slip.Symbol("element"), element)
		b = append(b, slip.ReadString(`(send element :name)`).Eval(scope, nil).(slip.String)...)
		b = append(b, ' ')
		b = append(b, slip.ReadString(`(send element :type)`).Eval(scope, nil).(slip.String)...)
		b = append(b, ' ')
		b = append(b, slip.ReadString(`(send element :path)`).Eval(scope, nil).(slip.String)...)
		b = append(b, '\n')
	}
	fmt.Printf("*** %s\n", b)

	// TBD schema should have a way to write as in the schema file

	// TBD other methods about the details of the file
}
