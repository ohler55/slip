// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWriteStream(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()

	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	result := slip.ReadString("(write 123 :stream out)").Eval(scope)

	tt.Equal(t, slip.Fixnum(123), result)
	tt.Equal(t, "123", out.String())
}

func TestWriteBaseRadix(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(write 123 :stream out :base 8 :radix t)").Eval(scope)

	tt.Equal(t, "#o173", out.String())
}

func TestWriteCase(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(write 'aBc :stream out :case nil)").Eval(scope)
	_ = slip.ReadString("(write 'aBc :stream out :case :upcase)").Eval(scope)

	tt.Equal(t, "aBcABC", out.String())
}

func TestWriteArray(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(write #2A((1 2)(3 4)) :stream out :array nil)").Eval(scope)
	_ = slip.ReadString("(write #2A((1 2)(3 4)) :stream out :array t)").Eval(scope)

	tt.Equal(t, "#<(ARRAY T (2 2))>#2A((1 2) (3 4))", out.String())
}

func TestWriteEscape(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(write #\\X :stream out :escape nil :circle t)").Eval(scope)
	_ = slip.ReadString("(write #\\X :stream out :escape t)").Eval(scope)

	tt.Equal(t, "X#\\X", out.String())
}

func TestWriteGensym(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(write 'abc :stream out :gensym t)").Eval(scope)

	// TBD try with (gensym)

	tt.Equal(t, "abc", out.String())
}

func TestWriteLength(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(write '(0 1 2 3 4 5) :stream out :length nil :pretty nil)").Eval(scope)
	_ = slip.ReadString("(write '(0 1 2 3 4 5) :stream out :length 4 :pretty nil)").Eval(scope)

	tt.Equal(t, "(0 1 2 3 4 5)(0 1 2 3 ...)", out.String())
}

func TestWriteLevel(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(write '(0 (1 (2 (3 (4))))) :stream out :level nil :pretty nil)").Eval(scope)
	_ = slip.ReadString("(write '(0 (1 (2 (3 (4))))) :stream out :level 3 :pretty nil)").Eval(scope)

	tt.Equal(t, "(0 (1 (2 (3 (4)))))(0 (1 (2 #)))", out.String())
}

func TestWriteLines(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString("(write '(0 (1 (2 (3 (4))))) :stream out :lines nil :right-margin 10 :pretty t)").Eval(scope)
	out.WriteString("\n")
	_ = slip.ReadString("(write '(0 (1 (2 (3 (4))))) :stream out :lines 4 :right-margin 10 :pretty t)").Eval(scope)

	tt.Equal(t, `(0
 (1
  (2
   (3
    (4)))))
(0
 (1
  (2
   (3 ..))))`, out.String())
}

func TestWriteReadably(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: &out})
	_ = slip.ReadString(`(write 1.2s+3 :stream out :readably nil :miser-width nil)`).Eval(scope)
	out.WriteString(" ")
	_ = slip.ReadString(`(write 1.2s+3 :stream out :readably t :miser-width 10)`).Eval(scope)

	tt.Equal(t, "1200 1.2s+03", out.String())
}

func TestWriteArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(write)`,
		Panics: true,
	}).Test(t)
}

func TestWriteBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(write 7 t t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(write 7 :bad t)`,
		Panics: true,
	}).Test(t)
}

func TestWriteBadBase(t *testing.T) {
	(&sliptest.Function{
		Source: `(write 7 :base t)`,
		Panics: true,
	}).Test(t)
}

func TestWriteBadCase(t *testing.T) {
	(&sliptest.Function{
		Source: `(write 7 :case :bad)`,
		Panics: true,
	}).Test(t)
}

func TestWriteBadStream(t *testing.T) {
	(&sliptest.Function{
		Source: `(write 7 :stream t)`,
		Panics: true,
	}).Test(t)
	scope := slip.NewScope()
	scope.Let(slip.Symbol("out"), &slip.OutputStream{Writer: badWriter(0)})
	tt.Panic(t, func() { _ = slip.ReadString("(write 7 :stream out)").Eval(scope) })
}

func TestWriteBadFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(write 7 :length -3)`,
		Panics: true,
	}).Test(t)
}
