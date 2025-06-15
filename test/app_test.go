// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

// A portion of the App functions can not be tested without actually running
// the App as a compiled standalone as the embed.FS is not available until the
// App is built. Plugins have a different issue around versions of source code
// that is one of the go "features".

func TestAppRunSimple(t *testing.T) {
	_ = os.RemoveAll("testdata/app/run-simple.out")
	scope := slip.NewScope()
	_ = slip.ReadString(`(fmakunbound 'app-quux)`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(fmakunbound 'app-no-op)`, scope).Eval(scope, nil)

	app := slip.App{
		Title: "quux",
		// Plugins: []string{"appplugin/appplugin.so"},
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

func TestAppRunKeyFile(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(fmakunbound 'app-quux)`, scope).Eval(scope, nil)
	_ = slip.ReadString(`(fmakunbound 'app-no-op)`, scope).Eval(scope, nil)

	app := slip.App{
		Title:         "quux",
		LispCode:      []string{"testdata/app/quux.lisp"},
		KeyFile:       "testdata/app/key", // a directory
		EntryFunction: "app-no-op",
		OnPanic: func(r any) int {
			fmt.Printf("*-*-* %s\n", r)
			return 1
		},
	}
	code := app.Run("")
	tt.Equal(t, 0, code)
}

func TestAppRunBadKeyFile(t *testing.T) {
	app := slip.App{
		Title:         "quux",
		LispCode:      []string{"testdata/app/quux.lisp"},
		KeyFile:       "testdata/app", // a directory
		EntryFunction: "app-no-op",
		OnPanic: func(r any) int {
			return 1
		},
	}
	code := app.Run()
	tt.Equal(t, 1, code)
}

func TestAppRunGenerate(t *testing.T) {
	// Use a directory outside the slip repo to avoid the clash of the slip
	// go.mod and the application go.mod.
	_ = os.RemoveAll("/tmp/scratch")
	defer func() { _ = os.RemoveAll("/tmp/scratch") }()
	app := slip.App{
		Title:   "quux",
		Plugins: []string{"appplugin/appplugin.so"},
		Options: []*slip.AppArg{
			{Flag: "num", Doc: "a number", Default: slip.Fixnum(0), Type: "fixnum", Var: "num"},
		},
		LispCode:      []string{"testdata/app/quux.lisp"},
		KeyFlag:       "key",
		KeyFile:       "testdata/app/key",
		EntryFunction: "app-quux",
		OnPanic: func(r any) int {
			fmt.Printf("*-*-* %s\n", r)
			return 1
		},
	}
	wd, _ := os.Getwd()
	code := app.Run("-key", "secret", "-slipapp.generate", "/tmp/scratch", "-slipapp.replace", filepath.Dir(wd))
	tt.Equal(t, 0, code)

	content, err := os.ReadFile("/tmp/scratch/go.mod")
	tt.Nil(t, err)
	tt.Equal(t, true, bytes.Contains(content, []byte("replace github.com/ohler55/slip =>")))
	tt.Equal(t, true, bytes.Contains(content, []byte("require github.com/ohler55/slip")))

	content, err = os.ReadFile("/tmp/scratch/main.go")
	tt.Nil(t, err)
	tt.Equal(t, true, bytes.Contains(content, []byte("func main() {")))
	tt.Equal(t, true, bytes.Contains(content, []byte("app.Run()")))
	tt.Equal(t, true, bytes.Contains(content, []byte("var srcFS embed.FS")))

	_, err = os.Stat("/tmp/scratch/go.sum")
	tt.Nil(t, err)
	_, err = os.Stat("/tmp/scratch/quux")
	tt.Nil(t, err)
	_, err = os.Stat("/tmp/scratch/src/app.lisp.enc")
	tt.Nil(t, err)
	_, err = os.Stat("/tmp/scratch/src/appplugin.so")
	tt.Nil(t, err)
}

func TestAppRunGenerateCleanup(t *testing.T) {
	// Use a directory outside the slip repo to avoid the clash of the slip
	// go.mod and the application go.mod.
	_ = os.RemoveAll("/tmp/scratch")
	defer func() { _ = os.RemoveAll("/tmp/scratch") }()
	app := slip.App{
		Title:   "quux",
		Plugins: []string{"appplugin/appplugin.so"},
		Options: []*slip.AppArg{
			{Flag: "num", Doc: "a number", Default: slip.Fixnum(0), Type: "fixnum", Var: "num"},
		},
		LispCode:      []string{"testdata/app/quux.lisp"},
		KeyFlag:       "key",
		KeyFile:       "testdata/app/key",
		EntryFunction: "app-quux",
		OnPanic: func(r any) int {
			fmt.Printf("*-*-* %s\n", r)
			return 1
		},
	}
	wd, _ := os.Getwd()
	code := app.Run(
		"-key", "secret",
		"-slipapp.generate", "/tmp/scratch",
		"-slipapp.replace", filepath.Dir(wd),
		"-slipapp.cleanup",
	)
	tt.Equal(t, 0, code)

	_, err := os.Stat("/tmp/scratch/quux")
	tt.Nil(t, err)

	_, err = os.Stat("/tmp/scratch/main.go")
	tt.NotNil(t, err)
	_, err = os.Stat("/tmp/scratch/go.mod")
	tt.NotNil(t, err)
	_, err = os.Stat("/tmp/scratch/go.sum")
	tt.NotNil(t, err)
	_, err = os.Stat("/tmp/scratch/src")
	tt.NotNil(t, err)
}

func TestAppRunPrepare(t *testing.T) {
	_ = os.RemoveAll("testdata/app/scratch")
	defer func() { _ = os.RemoveAll("testdata/app/scratch") }()
	app := slip.App{
		Title:   "quux",
		Plugins: []string{"appplugin/appplugin.so"},
		Options: []*slip.AppArg{
			{Flag: "num", Doc: "a number", Default: slip.Fixnum(0), Type: "fixnum", Var: "num"},
		},
		LispCode:      []string{"testdata/app/quux.lisp"},
		KeyFlag:       "key",
		KeyFile:       "testdata/app/key",
		EntryFunction: "app-quux",
		OnPanic: func(r any) int {
			fmt.Printf("*-*-* %s\n", r)
			return 1
		},
	}
	wd, _ := os.Getwd()
	code := app.Run("-key", "secret", "-slipapp.prepare", "testdata/app/scratch/src", "-slipapp.replace", filepath.Dir(wd))
	tt.Equal(t, 0, code)

	_, err := os.Stat("testdata/app/scratch/src/app.lisp.enc")
	tt.Nil(t, err)
	_, err = os.Stat("testdata/app/scratch/src/appplugin.so")
	tt.Nil(t, err)
}
