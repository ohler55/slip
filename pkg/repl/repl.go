// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime/debug"
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

	configHeader = ";;;; slip REPL configuration file. For help type: (help 'configuration)\n\n"
)

var (
	modifiedVars    = map[string]bool{}
	configFilename  = ""
	historyFilename = ""

	scope       slip.Scope
	prompt      string
	warnPrefix  string
	matchColor  string
	evalOnClose bool

	form1 slip.Object
	form2 slip.Object
	form3 slip.Object

	value1 slip.Object
	value2 slip.Object
	value3 slip.Object

	replReader reader = &termReader{}

	sizer hasSize
	// TBD flag to not let Run() be called more than once before (repl-exit) is called
)

func init() {
	if scope.Get(slip.Symbol(printANSI)) != nil {
		warnPrefix = "\x1b[31m"
		setPrompt(slip.String("\x1b[1;94m▶ \x1b[m"))
		matchColor = "\x1b[1m"
	} else {
		warnPrefix = ""
		setPrompt(slip.String("* "))
		matchColor = ""
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

// Die is used with panic to print an error and then exit.
type die string

// GetScope returns the REPL scope.
func GetScope() *slip.Scope {
	return &scope
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
	customPath := filepath.Join(dir, "custom.lisp")

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
	if buf, err = os.ReadFile(customPath); err == nil {
		code := slip.Read(buf)
		code.Compile()
		code.Eval(&scope)
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
		replReader.stop()
	}()
	replReader.initialize()
	for {
		process()
	}
}

// ZeroMods resets the modified variables list.
func ZeroMods() {
	modifiedVars = map[string]bool{}
}

func reset() {
	replReader.reset()
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
			replReader.setDepth(tr.Depth)
		case *slip.Panic:
			if 0 < len(tr.Message) {
				var buf []byte
				buf = append(buf, warnPrefix...)
				buf = append(buf, tr.Bytes()...)
				buf = append(buf, suffix...)
				_, _ = scope.Get(slip.Symbol(stdOutput)).(io.Writer).Write(buf)
			}
			reset()
			if scope.Get("*repl-debug*") != nil {
				debug.PrintStack()
			}
			if tr.Fatal {
				panic("")
			}
		case die:
			fmt.Fprintf(scope.Get(slip.Symbol(stdOutput)).(io.Writer), "%s%s%s\n", warnPrefix, tr, suffix)
			panic("")
		case error:
			if errors.Is(tr, io.EOF) {
				panic(nil) // exits the REPL loop
			}
			fmt.Fprintf(scope.Get(slip.Symbol(stdOutput)).(io.Writer), "%s%v%s\n", warnPrefix, tr, suffix)
			if scope.Get("*repl-debug*") != nil {
				debug.PrintStack()
			}
			reset()
		default:
			fmt.Fprintf(scope.Get(slip.Symbol(stdOutput)).(io.Writer), "%s%v%s\n", warnPrefix, tr, suffix)
			if scope.Get("*repl-debug*") != nil {
				debug.PrintStack()
			}
			reset()
		}
	}()
	buf := replReader.read()
	// Enter was pressed so save to history.
	replReader.addToHistory()

	code := slip.Read(buf)
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
		p := *slip.DefaultPrinter()
		p.Readably = true
		b = fmt.Appendf(b, "(setq %s ", key)
		b = p.Append(b, value, 0)
		b = append(b, ")\n"...)
	}
	if err := os.WriteFile(configFilename, b, 0666); err != nil {
		panic(err)
	}
}

func setHook(p *slip.Package, key string) {
	if p == &Pkg ||
		strings.HasPrefix(key, "*print-") ||
		key == "*bag-time-format*" || key == "*bag-time-wrap*" {
		modifiedVars[key] = true
		updateConfigFile()
	}
	replReader.addWord(key)
}

func unsetHook(p *slip.Package, key string) {
	replReader.removeWord(key)
}

func defunHook(p *slip.Package, key string) {
	replReader.addWord(key)
}

func getPrompt() slip.Object {
	return slip.String(prompt)
}

func setPrompt(value slip.Object) {
	if str, ok := value.(slip.String); ok {
		prompt = string(str)
		if ed, ok := replReader.(*editor); ok {
			ed.foff = printSize(prompt) + 1 // terminal positions are one based and not zero based so add one
		}
	} else {
		panic("*repl-prompt* must be a string")
	}
}

func getMatchColor() slip.Object {
	return slip.String(matchColor)
}

func setMatchColor(value slip.Object) {
	if str, ok := value.(slip.String); ok {
		matchColor = string(str)
	} else {
		panic("*repl-match-color* must be a string")
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

func getEditor() slip.Object {
	if _, ok := replReader.(*editor); ok {
		return slip.True
	}
	return nil
}

func setEditor(value slip.Object) {
	if value == nil {
		if _, ok := replReader.(*termReader); !ok {
			replReader = &termReader{}
			replReader.initialize()
		}
	} else if _, ok := replReader.(*editor); !ok {
		replReader = &editor{}
		replReader.initialize()
	}
}

func getEvalOnClose() slip.Object {
	if evalOnClose {
		return slip.True
	}
	return nil
}

func setEvalOnClose(value slip.Object) {
	evalOnClose = value != nil
}

func SetSizer(hs hasSize) {
	sizer = hs
}

func getREPL() slip.Object {
	return &Pkg
}

func setREPL(_ slip.Object) {
	panic("*repl* is a read only variable")
}
