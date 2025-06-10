// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"embed"
	"flag"
	"fmt"
	"os"
)

// Plugin specifies a package and optionally the version.
type Plugin struct {
	Name    string
	Version string
}

// CmdArg are used to specify then variables that will be set from the command
// line arguments.
type CmdArg struct {
	// Flag is the command line flag such as 'v' which matches '-v' on the
	// command line.
	Flag string

	// Doc is the documentation or description of the flag as it appears in
	// the help display that is triggered a '-h' on the command line.
	Doc string

	// Default is the default SLIP object that will be assigned to the
	// variable associated with the flag unless over-ridden by a command line
	// option.
	Default Object

	// Type is the SLIP type to coerce the command line option value in to.
	Type string

	// Var is the name of the variable to that is bound to the flag value.
	Var string

	// Value is the string value from the command line.
	Value string
}

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

	// Plugins are the plugin packages to be included in the go.mod file.
	Plugins []*Plugin

	// Options identify the command line options for the application.
	Options []*CmdArg

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
	if app.OnPanic == nil {
		app.OnPanic = func(r any) int {
			fmt.Printf("*-*-* %s\n", r)
			return 1
		}
	}
	defer func() {
		if r := recover(); r != nil {
			exitCode = app.OnPanic(r)
		}
	}()
	fs := flag.CommandLine
	if 0 < len(args) {
		fs = flag.NewFlagSet(app.Title, flag.ExitOnError)
	} else {
		args = os.Args
	}
	if app.Usage != nil {
		fs.Usage = app.Usage
	}
	scope := NewScope()

	var key string

	if 0 < len(app.KeyFlag) {
		fs.StringVar(&key, app.KeyFlag, "", "source code decryption key")
	}
	app.setupFlags(fs)
	if err := fs.Parse(args); err != nil {
		panic(err)
	}
	fmt.Printf("*** key: %s\n", key)

	app.updateScopeFromFlags(scope)
	fmt.Printf("*** scope: %v\n", scope.Vars)

	// update scope with flags

	// TBD setup flags and parse
	// set vars from flag parse

	app.load(scope, key)

	_ = ReadString(fmt.Sprintf("(%s)", app.EntryFunction), scope).Eval(scope, nil)

	return
}

func (app *App) setupFlags(fs *flag.FlagSet) {
	// TBD consider type for setup
	// bool, int, float, string
	for _, opt := range app.Options {
		fs.StringVar(&opt.Value, opt.Flag, ObjectString(opt.Default), opt.Doc)
	}
}

func (app *App) updateScopeFromFlags(scope *Scope) {
	// TBD
	for _, opt := range app.Options {
		fmt.Printf("*** %s: %s\n", opt.Var, opt.Value)
		scope.Let(Symbol(opt.Var), Coerce(String(opt.Value), Symbol(opt.Type)))
	}
}

func (app *App) Generate(dir, slipVersion string) {
	// TBD create the dir if needed
	// generate a app.lisp file, encrypt if a key is provided
	// write a main.go file
	// go mod init
	// go build (verify it builds)
}

func (app *App) load(scope *Scope, key string) {
	if app.Source != nil {
		// TBD load embedded lisp
		return
	}
	for _, path := range app.LispCode {
		content, err := os.ReadFile(path)
		if err != nil {
			panic(err)
		}
		_ = Compile(content, scope)
	}
}
