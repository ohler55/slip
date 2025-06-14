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
    	a string (default "\"default\"")
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
