// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestAppRunSimple(t *testing.T) {
	app := slip.App{
		Title:   "quux",
		Plugins: []*slip.Plugin{},
		Options: []*slip.CmdArg{
			{Flag: "sym", Doc: "a symbol", Default: nil, Type: "symbol", Var: "sym"},
		},
		LispCode:      []string{"testdata/app/quux.lisp"},
		Source:        nil,
		KeyFlag:       "key",
		KeyFile:       "testdata/app/key",
		EntryFunction: "quux",
	}

	// TBD add flagset and then use vars in quux.lisp

	code := app.Run("-key", "secret-key", "-sym", "cymbal")
	tt.Equal(t, 0, code)
	out, err := os.ReadFile("testdata/app/run-simple.out")
	tt.Nil(t, err)
	tt.Equal(t, "This is the output for the AppRunSimple test.\n", string(out))
}

// func TestAppGenerate(t *testing.T) {
// 	app := slip.App{
// 		Title:         "quux",
// 		Plugins:       []*slip.Plugin{},
// 		Options:       []*slip.CmdArg{},
// 		LispCode:      []string{},
// 		Source:        nil,
// 		KeyFlag:       "key",
// 		KeyFile:       "testdata/key",
// 		EntryFunction: "quux",
// 	}

// 	app.Generate("testdate/quux", "latest")

// 	// TBD verify main.go
// }
