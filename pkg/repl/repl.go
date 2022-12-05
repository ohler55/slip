// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

const (
	printANSI = "*print-ansi*"
	stdInput  = "*standard-input*"
	stdOutput = "*standard-output*"

	form1Key  = "+"
	form2Key  = "++"
	form3Key  = "+++"
	value1Key = "/"
	value2Key = "//"
	value3Key = "///"

	configHeader = ";;;; slip REPL configuration file. For help type: (help configuration)\n\n"
)

var (
	modifiedVars = map[string]bool{}
	// TBD is a lock file needed to avoid collisions when running multiple
	// instances?
	configFilename  = ""
	historyFilename = ""

	scope      slip.Scope
	prompt     string
	warnPrefix string

	form1 slip.Object
	form2 slip.Object
	form3 slip.Object

	value1 slip.Object
	value2 slip.Object
	value3 slip.Object

	lineBuf   = make([]byte, 1024)
	formBuf   []byte
	formDepth int

	// TBD flag to not let Run() be called more than once before (repl-exit) is called
)

func init() {
	if scope.Get(slip.Symbol(printANSI)) != nil {
		warnPrefix = "\x1b[31m"
		prompt = "\x1b[1;94m▶ \x1b[m"
	} else {
		warnPrefix = ""
		prompt = "* "
	}
	scope.Let(slip.Symbol(form1Key), nil)
	scope.Let(slip.Symbol(form2Key), nil)
	scope.Let(slip.Symbol(form3Key), nil)
	scope.Let(slip.Symbol(value1Key), nil)
	scope.Let(slip.Symbol(value2Key), nil)
	scope.Let(slip.Symbol(value3Key), nil)

	slip.SetHook = setHook
	slip.UnsetHook = unsetHook
	slip.DefunHook = defunHook
}

// SetConfigDir sets the configuration directory for the configuration and
// history files.
func SetConfigDir(dir string) {
	if strings.Contains(dir, "~") {
		home, err := os.UserHomeDir()
		if err != nil {
			panic(err)
		}
		dir = strings.ReplaceAll(dir, "~", home)
	}
	var err error
	if dir, err = filepath.Abs(filepath.Clean(dir)); err != nil {
		panic(err)
	}
	cfgPath := filepath.Join(dir, "config.lisp")
	historyFilename = filepath.Join(dir, "history")

	_ = os.MkdirAll(dir, 0755)
	var buf []byte
	if buf, err = os.ReadFile(cfgPath); err == nil {
		configFilename = "" // Turn off writing while evaluating config file.
		code := slip.Read(buf)
		code.Compile()
		code.Eval(&scope)
	} else {
		if os.IsNotExist(err) {
			if err = os.WriteFile(cfgPath, []byte(configHeader), 0666); err != nil {
				panic(err)
			}
		} else {
			panic(err)
		}
	}
	configFilename = cfgPath
}

// Scope returns the REPL scope.
func Scope() *slip.Scope {
	return &scope
}

// Run starts the REPL.
func Run() {
	defer func() {
		_ = recover()
		_, _ = scope.Get(slip.Symbol(stdOutput)).(io.Writer).Write([]byte("\nBye\n"))
	}()
	for {
		process()
	}
}

// ZeroMods resets the modified variables list.
func ZeroMods() {
	modifiedVars = map[string]bool{}
}

func reset() {
	formBuf = formBuf[:0]
	formDepth = 0
}

func process() {
	defer func() {
		rec := recover()
		if rec == nil {
			reset()
			return
		}
		var suffix string

		if strings.Contains(warnPrefix, "\u001b") {
			suffix = "\x1b[m"
		}
		switch tr := rec.(type) {
		case *slip.Partial:
			formDepth = tr.Depth
		case *slip.Panic:
			var buf []byte
			buf = append(buf, warnPrefix...)
			buf = append(buf, tr.Bytes()...)
			buf = append(buf, suffix...)
			_, _ = scope.Get(slip.Symbol(stdOutput)).(io.Writer).Write(buf)
			reset()
			// debug.PrintStack()
		case error:
			if errors.Is(tr, io.EOF) {
				panic(nil) // exits the REPL loop
			}
			// debug.PrintStack()
			fmt.Fprintf(scope.Get(slip.Symbol(stdOutput)).(io.Writer), "%s%v%s\n", warnPrefix, tr, suffix)
			reset()
		default:
			// debug.PrintStack()
			fmt.Fprintf(scope.Get(slip.Symbol(stdOutput)).(io.Writer), "%s%v%s\n", warnPrefix, tr, suffix)
			reset()
		}
	}()
	read()
	code := slip.Read(formBuf)
	for _, obj := range code {
		var skipWrite bool

		form3 = form2
		form2 = form1
		form1 = obj

		value3 = value2
		value2 = value1
		value1 = obj.Eval(&scope, 0)
		if value1 == slip.Novalue {
			value1 = nil
			skipWrite = true
		}
		// Eval was successful.
		writeToHistory()

		scope.Set(slip.Symbol(form1Key), form1)
		scope.Set(slip.Symbol(form2Key), form2)
		scope.Set(slip.Symbol(form3Key), form3)

		scope.Set(slip.Symbol(value1Key), value1)
		scope.Set(slip.Symbol(value2Key), value2)
		scope.Set(slip.Symbol(value3Key), value3)

		if !skipWrite {
			fmt.Fprintf(scope.Get(slip.Symbol(stdOutput)).(io.Writer), "%s\n", slip.ObjectString(value1))
		}
	}
}

func read() {
	if 0 < len(formBuf) {
		_, _ = scope.Get(slip.Symbol(stdOutput)).(io.Writer).Write([]byte("  "))
	} else {
		_, _ = scope.Get(slip.Symbol(stdOutput)).(io.Writer).Write([]byte(prompt))
	}
	for {
		n, err := scope.Get(slip.Symbol(stdInput)).(io.Reader).Read(lineBuf)
		if err != nil {
			panic(err)
		}
		formBuf = append(formBuf, lineBuf[:n]...)
		if 0 < len(formBuf) && formBuf[len(formBuf)-1] == '\n' {
			break
		}
	}
}

func updateConfigFile() {
	if len(configFilename) == 0 || len(modifiedVars) == 0 {
		return
	}
	var b []byte
	b = append(b, configHeader...)
	keys := make([]string, 0, len(modifiedVars))
	for key := range modifiedVars {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	for _, key := range keys {
		value := slip.UserPkg.JustGet(key)
		b = fmt.Appendf(b, "(setq %s %s)\n", key, slip.ObjectString(value))
	}
	if err := os.WriteFile(configFilename, b, 0666); err != nil {
		panic(err)
	}
}

func writeToHistory() {
	// TBD write formBuf to history
}

func setHook(p *slip.Package, key string) {
	if p == &Pkg ||
		strings.HasPrefix(key, "*print-") ||
		key == "*bag-time-format*" || key == "*bag-time-wrap*" {
		modifiedVars[key] = true
		updateConfigFile()
	}
	// TBD add to completions
}

func unsetHook(p *slip.Package, key string) {
	// TBD remove from completions
}

func defunHook(p *slip.Package, key string) {
	// TBD
}

func getPrompt() slip.Object {
	return slip.String(prompt)
}

func setPrompt(value slip.Object) {
	if str, ok := value.(slip.String); ok {
		prompt = string(str)
	} else {
		panic("*repl-prompt* must be a string")
	}
}

func getWarnPrefix() slip.Object {
	return slip.String(warnPrefix)
}

func setWarnPrefix(value slip.Object) {
	if str, ok := value.(slip.String); ok {
		warnPrefix = string(str)
	} else {
		panic("*repl-warning-prefix* must be a string")
	}
}
