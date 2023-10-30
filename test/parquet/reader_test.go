// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/pretty"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
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

	for _, method := range []string{
		":init",
		":close",
		":version",
		":created-by",
		":column-count",
		":columns",
		":column",
		":row-count",
		":rows",
		":each-row",
		":schema",
	} {
		_ = slip.ReadString(fmt.Sprintf(`(describe-method parquet-reader-flavor %s out)`, method)).Eval(scope, nil)
		tt.Equal(t, true, strings.Contains(out.String(), method))
	}
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
			vlist := v.(slip.List)
			// TBD not correct, print with %v
			for i, x := range []string{
				`(("a" . (1 . t)) ("b" . (2 . nil)) ("c" . (1 . t)) ("d" . (1 . t)) ("e" . (3 . t)) ("f" . (4 . nil)))`,
				`(1 1 1 1 1 1)`,
				`(1 1 1 1 1 1)`,
			} {
				tt.Equal(t, x, slip.ObjectString(vlist[i]))
			}
		},
	}).Test(t)
}

func TestReaderColumnsNestedStruct(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(
		`(make-instance 'parquet-reader-flavor :file "testdata/nested-struct.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :columns)`,
		Validate: func(t *testing.T, v slip.Object) {
			vlist := v.(slip.List)
			tt.Equal(t, 36, len(vlist))
			// Pick a few at random and verify the contents.
			// Order is arbitrary since a map is created first so convert
			// back to a map and use pretty.SEN to sort the output.
			tt.Equal(t, "[{count: 495 max: 742 mean: 416 min: 115 sum: 206195 variance: 10374}]",
				pretty.SEN(bag.ObjectToBag(vlist[1]), 100.3))
			tt.Equal(t, "[{count: 495 max: 0 mean: 0 min: 0 sum: 0 variance: 0}]",
				pretty.SEN(bag.ObjectToBag(vlist[20]), 100.3))
			tt.Equal(t, "[{count: 81 max: 100 mean: 1.2345679012345678 min: 0 sum: 100 variance: 123.45679012345684}]",
				pretty.SEN(bag.ObjectToBag(vlist[26]), 100.3))
		},
	}).Test(t)
}

func TestReaderColumnsBinary(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(
		`(make-instance 'parquet-reader-flavor :file "testdata/binary.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	scope.Set("*print-readably*", slip.True)
	defer func() {
		_ = slip.ReadString(`(progn (setq *print-readably* nil)(send reader :close))`).Eval(scope, nil)
	}()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :columns)`,
		Validate: func(t *testing.T, v slip.Object) {
			vlist := v.(slip.List)
			tt.Equal(t,
				`("\u0000" "\u0001" "\u0002" "\u0003" "\u0004" "\u0005" "\u0006" "\u0007" "\b" "\t" "\n" "\u000b")`,
				slip.ObjectString(vlist[0]))
		},
	}).Test(t)
}

func TestReaderColumnFixnum(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :column 0)`,
		Validate: func(t *testing.T, v slip.Object) {
			tt.Equal(t, "(4 5 6 7 2 3 0 1)", slip.ObjectString(v))
		},
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send reader :column 1.5)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestReaderColumnNamed(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :column "id")`,
		Validate: func(t *testing.T, v slip.Object) {
			tt.Equal(t, "(4 5 6 7 2 3 0 1)", slip.ObjectString(v))
		},
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :column 'id)`,
		Validate: func(t *testing.T, v slip.Object) {
			tt.Equal(t, "(4 5 6 7 2 3 0 1)", slip.ObjectString(v))
		},
	}).Test(t)
}

func TestReaderColumnErrors(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send reader :column 1.5)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send reader :column)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestReaderRowsPrimitive(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	scope.Set("*print-right-margin*", slip.Fixnum(80))
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :rows :assoc)`,
		Validate: func(t *testing.T, v slip.Object) {
			vlist := v.(slip.List)
			tt.Equal(t, 8, len(vlist))
			tt.Equal(t,
				`(("id" . 4) ("bool_col" . t) ("tinyint_col" . 0) ("smallint_col" . 0) ("int_col" . 0)
          ("bigint_col" . 0) ("float_col" . 0) ("double_col" . 0)
          ("date_string_col" . "03/01/09") ("string_col" . "0")
          ("timestamp_col" . @2009-03-01T00:00:00Z))`,
				vlist[0].String())
		},
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :rows :list)`,
		Validate: func(t *testing.T, v slip.Object) {
			vlist := v.(slip.List)
			tt.Equal(t, 8, len(vlist))
			tt.Equal(t, `(4 t 0 0 0 0 0 0 "03/01/09" "0" @2009-03-01T00:00:00Z)`, vlist[0].String())
		},
	}).Test(t)

	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :rows :assoc '(0 int_col "bool_col"))`,
		Validate: func(t *testing.T, v slip.Object) {
			vlist := v.(slip.List)
			tt.Equal(t, 8, len(vlist))
			tt.Equal(t, `(("id" . 4) ("int_col" . 0) ("bool_col" . t))`, vlist[0].String())
		},
	}).Test(t)
}

func TestReaderRowsNested(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(
		`(make-instance 'parquet-reader-flavor :file "testdata/nested-maps.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	scope.Set("*print-right-margin*", slip.Fixnum(80))
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :rows :list)`,
		Validate: func(t *testing.T, v slip.Object) {
			vlist := v.(slip.List)
			tt.Equal(t, `((t 1 1) (nil 1 1) (t 1 1) (t 1 1) (t 1 1) (nil 1 1))`, vlist.String())
		},
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(send reader :rows :assoc)`,
		Validate: func(t *testing.T, v slip.Object) {
			vlist := v.(slip.List)
			for i, x := range []string{
				// `(("a" . ("a" . ((1 . t) (2 . nil))) ("b" . 1) ("c" . 1))`,
				`(("a" . ("a" . (1 . t))) ("b" . 1) ("c" . 1))`,
				`(("a" . ("b" . (2 . nil))) ("b" . 1) ("c" . 1))`,
				`(("a" . ("c" . (1 . t))) ("b" . 1) ("c" . 1))`,
				`(("a" . ("d" . (1 . t))) ("b" . 1) ("c" . 1))`,
				`(("a" . ("e" . (3 . t))) ("b" . 1) ("c" . 1))`,
				`(("a" . ("f" . (4 . nil))) ("b" . 1) ("c" . 1))`,
			} {
				tt.Equal(t, x, slip.ObjectString(vlist[i]))
			}
		},
	}).Test(t)
}

func TestReaderRowsError(t *testing.T) {
	scope := slip.NewScope()
	pr := slip.ReadString(`(make-instance 'parquet-reader-flavor :file "testdata/primitive.parquet")`).Eval(scope, nil)
	scope.Let("reader", pr)
	defer func() { _ = slip.ReadString(`(send reader :close)`).Eval(scope, nil) }()

	(&sliptest.Function{
		Scope:     scope,
		Source:    `(send reader :rows :what)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
