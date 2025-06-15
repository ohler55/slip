// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"flag"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestAppArgString(t *testing.T) {
	aa := slip.AppArg{
		Flag:    "f",
		Doc:     "a string",
		Default: slip.String("default"),
		Type:    "string",
		Var:     "flag-value",
	}
	var sb strings.Builder
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	fs.SetOutput(&sb)
	scope := slip.NewScope()
	aa.SetFlag(fs, scope)

	err := fs.Parse([]string{"-f", "quux"})
	tt.Nil(t, err)

	fs.PrintDefaults()
	tt.Equal(t, `  -f string
    	a string (default "default")
`, sb.String())

	aa.UpdateScope(scope)
	tt.Equal(t, slip.String("quux"), scope.Get("flag-value"))

	tt.Equal(t, `slip.String("default")`, aa.DefaultReadable())
}

func TestAppArgInt(t *testing.T) {
	aa := slip.AppArg{
		Flag:    "f",
		Doc:     "an int",
		Default: slip.Fixnum(1),
		Type:    "fixnum",
		Var:     "flag-value",
	}
	var sb strings.Builder
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	fs.SetOutput(&sb)
	scope := slip.NewScope()
	aa.SetFlag(fs, scope)

	err := fs.Parse([]string{"-f", "7"})
	tt.Nil(t, err)

	fs.PrintDefaults()
	tt.Equal(t, `  -f int
    	an int (default 1)
`, sb.String())

	aa.UpdateScope(scope)
	tt.Equal(t, slip.Fixnum(7), scope.Get("flag-value"))

	tt.Equal(t, `slip.Fixnum(1)`, aa.DefaultReadable())
}

func TestAppArgFloat(t *testing.T) {
	aa := slip.AppArg{
		Flag:    "f",
		Doc:     "a float",
		Default: slip.DoubleFloat(1.5),
		Type:    "float",
		Var:     "flag-value",
	}
	var sb strings.Builder
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	fs.SetOutput(&sb)
	scope := slip.NewScope()
	aa.SetFlag(fs, scope)

	err := fs.Parse([]string{"-f", "7.5"})
	tt.Nil(t, err)

	fs.PrintDefaults()
	tt.Equal(t, `  -f float
    	a float (default 1.5)
`, sb.String())

	aa.UpdateScope(scope)
	tt.Equal(t, slip.DoubleFloat(7.5), scope.Get("flag-value"))

	tt.Equal(t, `slip.DoubleFloat(1.5)`, aa.DefaultReadable())
}

func TestAppArgSymbol(t *testing.T) {
	aa := slip.AppArg{
		Flag:    "f",
		Doc:     "a symbol",
		Default: slip.Symbol("buux"),
		Type:    "symbol",
		Var:     "flag-value",
	}
	var sb strings.Builder
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	fs.SetOutput(&sb)
	scope := slip.NewScope()
	aa.SetFlag(fs, scope)

	err := fs.Parse([]string{"-f", "quux"})
	tt.Nil(t, err)

	fs.PrintDefaults()
	tt.Equal(t, `  -f string
    	a symbol (default "buux")
`, sb.String())

	aa.UpdateScope(scope)
	tt.Equal(t, slip.Symbol("quux"), scope.Get("flag-value"))

	tt.Equal(t, `slip.Symbol("buux")`, aa.DefaultReadable())
}

func TestAppArgSymbolNil(t *testing.T) {
	aa := slip.AppArg{
		Flag:    "f",
		Doc:     "a symbol",
		Default: nil,
		Type:    "symbol",
		Var:     "flag-value",
	}
	var sb strings.Builder
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	fs.SetOutput(&sb)
	scope := slip.NewScope()
	aa.SetFlag(fs, scope)

	aa.UpdateScope(scope)
	tt.Equal(t, nil, scope.Get("flag-value"))

	err := fs.Parse([]string{"-f", "quux"})
	tt.Nil(t, err)

	fs.PrintDefaults()
	tt.Equal(t, `  -f string
    	a symbol
`, sb.String())

	aa.UpdateScope(scope)
	tt.Equal(t, slip.Symbol("quux"), scope.Get("flag-value"))

	tt.Equal(t, `nil`, aa.DefaultReadable())
}

func TestAppArgBooleanTrue(t *testing.T) {
	aa := slip.AppArg{
		Flag:    "f",
		Doc:     "a boolean",
		Default: nil,
		Type:    "boolean",
		Var:     "flag-value",
	}
	var sb strings.Builder
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	fs.SetOutput(&sb)
	scope := slip.NewScope()
	aa.SetFlag(fs, scope)

	err := fs.Parse([]string{"-f"})
	tt.Nil(t, err)

	fs.PrintDefaults()
	tt.Equal(t, "  -f\ta boolean\n", sb.String())

	aa.UpdateScope(scope)
	tt.Equal(t, slip.True, scope.Get("flag-value"))

	tt.Equal(t, `nil`, aa.DefaultReadable())
}

func TestAppArgBooleanFalse(t *testing.T) {
	aa := slip.AppArg{
		Flag:    "f",
		Doc:     "a boolean",
		Default: slip.True,
		Type:    "boolean",
		Var:     "flag-value",
	}
	var sb strings.Builder
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	fs.SetOutput(&sb)
	scope := slip.NewScope()
	aa.SetFlag(fs, scope)

	err := fs.Parse([]string{"-f=false"})
	tt.Nil(t, err)

	fs.PrintDefaults()
	tt.Equal(t, "  -f\ta boolean (default true)\n", sb.String())

	aa.UpdateScope(scope)
	tt.Nil(t, scope.Get("flag-value"))

	tt.Equal(t, `t`, aa.DefaultReadable())
}

func TestAppArgOctets(t *testing.T) {
	aa := slip.AppArg{
		Flag:    "f",
		Doc:     "an octets",
		Default: slip.Octets("abc"),
		Type:    "octets",
		Var:     "flag-value",
	}
	var sb strings.Builder
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	fs.SetOutput(&sb)
	scope := slip.NewScope()
	aa.SetFlag(fs, scope)

	err := fs.Parse([]string{"-f", "quux"})
	tt.Nil(t, err)

	fs.PrintDefaults()
	tt.Equal(t, `  -f string
    	an octets (default "abc")
`, sb.String())

	aa.UpdateScope(scope)
	tt.Equal(t, slip.Octets("quux"), scope.Get("flag-value"))

	tt.Equal(t, `slip.Octets("abc")`, aa.DefaultReadable())
}

func TestAppArgEvalBefore(t *testing.T) {
	aa := slip.AppArg{
		Flag:    "f",
		Doc:     "some code",
		Default: nil,
		Type:    "eval-before",
		Var:     "flag-value",
	}
	an := slip.AppArg{
		Flag:    "num",
		Default: slip.Fixnum(3),
		Type:    "fixnum",
		Var:     "num",
	}
	var sb strings.Builder
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	fs.SetOutput(&sb)
	scope := slip.NewScope()
	scope.Let(slip.Symbol("num"), slip.Fixnum(1))
	aa.SetFlag(fs, scope)
	an.SetFlag(fs, scope)

	err := fs.Parse([]string{"-f", "(1+ num)", "-num", "5"})
	tt.Nil(t, err)

	fs.PrintDefaults()
	tt.Equal(t, `  -f value
    	some code
  -num int
    	 (default 3)
`, sb.String())

	aa.UpdateScope(scope)
	tt.Equal(t, slip.Fixnum(2), scope.Get("flag-value"))

	tt.Equal(t, `nil`, aa.DefaultReadable())
}

func TestAppArgEvalAfter(t *testing.T) {
	aa := slip.AppArg{
		Flag:    "f",
		Doc:     "some code",
		Default: nil,
		Type:    "eval-after",
		Var:     "flag-value",
	}
	an := slip.AppArg{
		Flag:    "num",
		Default: slip.Fixnum(3),
		Type:    "fixnum",
		Var:     "num",
	}
	var sb strings.Builder
	fs := flag.NewFlagSet("test", flag.ContinueOnError)
	fs.SetOutput(&sb)
	scope := slip.NewScope()
	scope.Let(slip.Symbol("num"), slip.Fixnum(1))
	aa.SetFlag(fs, scope)
	an.SetFlag(fs, scope)

	err := fs.Parse([]string{"-f", "(1+ num)", "-num", "5"})
	tt.Nil(t, err)

	fs.PrintDefaults()
	tt.Equal(t, `  -f value
    	some code
  -num int
    	 (default 3)
`, sb.String())

	an.UpdateScope(scope)
	aa.UpdateScope(scope)
	tt.Equal(t, slip.Fixnum(6), scope.Get("flag-value"))

	tt.Equal(t, `nil`, aa.DefaultReadable())
}
