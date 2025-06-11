// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"embed"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"plugin"
	"strings"
)

// App has multiple roles related to developing, generating, and running an
// application. During development of a standalone SLIP application the App
// can execute LISP code from source files. Once development is complete then
// App can be used to generate a standalone application project. When that
// application is run the App functions assist in setting up and evaluating
// the LISP code that implements the application behavior.
type App struct {
	// Title is the name of the application.
	Title string

	// Usage for help documentation.
	Usage func()

	// Plugins is a list of paths to the plugin packages to be included in the
	// go.mod file. (e.g., src/message.so)
	Plugins []string

	// Options identify the command line options for the application.
	Options []*AppArg

	// LispCode are the filepaths to the LISP source code. This use used
	// during development and to form an encrypted and embedded file when the
	// application is generated.
	LispCode []string

	// Source for the application. This is expected to be a single LISP file
	// that may be encryped with a key specified as a command line argument or
	// in a file.
	Source *embed.FS

	// KeyFlag is the command line flag for specifying a decryption key if the
	// source is encrypted.
	KeyFlag string

	// KeyFile is the path to the key file containing a decryption key if one
	// is needed.
	KeyFile string

	// EntryFunction is the name of a function or a lambda that takes no
	// arguments. This is called to run the application. It is started within
	// a scope that includes the command line argument variables already
	// bound.
	EntryFunction string

	// OnPanic is called if a panic is raised. It should return the exit code
	// for the application.
	OnPanic func(f any) int
}

// Run the application with the optional arguments. The args provide a means
// of testing during development.
func (app *App) Run(args ...string) (exitCode int) {
	defer func() {
		if r := recover(); r != nil {
			exitCode = 1
			if app.OnPanic != nil {
				exitCode = app.OnPanic(r)
			}
		}
	}()
	fs := flag.CommandLine
	if 0 < len(args) {
		fs = flag.NewFlagSet(app.Title, flag.ExitOnError)
	} else {
		args = os.Args[1:]
	}
	if app.Usage != nil {
		fs.Usage = app.Usage
	}
	scope := NewScope()

	var key string
	if 0 < len(app.KeyFlag) {
		fs.StringVar(&key, app.KeyFlag, "", "source code decryption key")
	}
	for _, opt := range app.Options {
		opt.SetFlag(fs, scope)
	}
	if err := fs.Parse(args); err != nil {
		panic(err)
	}
	for _, opt := range app.Options {
		opt.UpdateScope(scope)
	}
	app.load(scope, key)
	_ = ReadString(fmt.Sprintf("(%s)", app.EntryFunction), scope).Eval(scope, nil)

	return
}

// BuildEmbed sources and plugin libraries to the specified directory. If a
// non empty key is provided then lisp sources are encrypted.
func (app *App) BuildEmbed(dir, key string) {
	// TBD create dir and encrypt lisp code and place in dir along with required plugin .so files
}

// Generate project directory with a main.go, go.mod, and lisp code directory.
func (app *App) Generate(dir, slipVersion string) {
	// TBD create the dir if needed
	// generate a app.lisp file, encrypt if a key is provided
	// write a main.go file
	// go mod init
	// go build (verify it builds)
}

func (app *App) load(scope *Scope, key string) {
	if app.Source != nil {
		entries, err := app.Source.ReadDir("src")
		if err != nil {
			panic(err)
		}
		for _, entry := range entries {
			path := filepath.Join("src", entry.Name())
			switch {
			case strings.HasSuffix(path, ".lisp"):
				content, err := app.Source.ReadFile(path)
				if err != nil {
					panic(err)
				}
				_ = Compile(content, scope)
			case strings.HasSuffix(path, ".so"):
				// TBD create a temp file and write the .so into the file then open that file
				// can the file be deleted after that? I hope so

				// For testing leave in place so it picks up the original file
				if _, err := plugin.Open(path); err != nil {
					NewPanic("plugin %s open failed. %s", path, err)
				}
			case strings.HasSuffix(path, ".enc"):
				// TBD decrypt with key, panic if no key
				fmt.Printf("*** %s is encrypted\n", path)
			}
		}
		return
	}
	for _, path := range app.Plugins {
		if _, err := plugin.Open(path); err != nil {
			NewPanic("plugin %s open failed. %s", path, err)
		}
	}
	for _, path := range app.LispCode {
		content, err := os.ReadFile(path)
		if err != nil {
			panic(err)
		}
		_ = Compile(content, scope)
	}
}
