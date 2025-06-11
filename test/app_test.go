// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestAppRunSimple(t *testing.T) {
	app := slip.App{
		Title: "quux",
		//Plugins: []string{"appplugin/appplugin.so"},
		Options: []*slip.AppArg{
			{Flag: "sym", Doc: "a symbol", Default: nil, Type: "symbol", Var: "sym"},
			{Flag: "num", Doc: "a number", Default: slip.Fixnum(0), Type: "fixnum", Var: "num"},
			{Flag: "str", Doc: "a string", Default: slip.String(""), Type: "string", Var: "str"},
			{Flag: "boo", Doc: "boolean", Default: nil, Type: "boolean", Var: "boo"},
			{Flag: "any", Doc: "anything", Default: nil, Type: "", Var: "any"},
		},
		LispCode:      []string{"testdata/app/quux.lisp"},
		Source:        nil,
		KeyFlag:       "key",
		KeyFile:       "testdata/app/key",
		EntryFunction: "app-quux",
		OnPanic: func(r any) int {
			fmt.Printf("*-*-* %s\n", r)
			return 1
		},
	}
	code := app.Run("-key", "secret-key", "-sym", "cymbal", "-num", "3", "-any", "(1 2 3)", "-str", "string", "-boo")
	tt.Equal(t, 0, code)
	out, err := os.ReadFile("testdata/app/run-simple.out")
	tt.Nil(t, err)
	tt.Equal(t, "The variables from AppRunSimple are boo: t num: 3 sym: cymbal str: string any: (1 2 3).\n",
		string(out))
}
