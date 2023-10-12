// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

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
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :repetition out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":repetition"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :max-definitions out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":max-definitions"))

	out.Reset()
	_ = slip.ReadString(`(describe-method parquet-schema-flavor :write out)`).Eval(scope, nil)
	tt.Equal(t, true, strings.Contains(out.String(), ":write"))
}
