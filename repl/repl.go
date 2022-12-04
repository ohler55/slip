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
	"github.com/ohler55/slip/pkg/flavors"
)

const (
	bold = "\x1b[1m"

	printANSI = "*print-ansi*"
	stdInput  = "*standard-input*"
	stdOutput = "*standard-output*"

	// TBD make these vars on repl instance
	// TBD set warning color (list colors or none)
	warnANSIKey = "*repl-warning-ansi*"

	form1Key  = "+"
	form2Key  = "++"
	form3Key  = "+++"
	value1Key = "/"
	value2Key = "//"
	value3Key = "///"

	configHeader = ";;;; slip REPL configuration file\n\n"
)

var (
	// A value of true is setq while false is for (send *repl* set-var value)
	modifiedVars = map[string]bool{}
	// TBD is a lock file needed to avoid collisions when running multiple
	// instances?
	configFilename  = ""
	historyFilename = ""

	theOne loop
)

func init() {
	theOne.line = make([]byte, 1024)

	rf := flavors.DefFlavor("REPL", map[string]slip.Object{}, []string{}, slip.List{})
	theOne.Flavor = rf
	theOne.Vars = map[string]slip.Object{"self": &theOne}

	// TBD setup like flavors.MakeInstance

	if theOne.Get(slip.Symbol(printANSI)) != nil {
		theOne.Let(slip.Symbol(warnANSIKey), slip.String("\x1b[31m"))
		theOne.prompt = "\x1b[1;94m▶ \x1b[m"
	} else {
		theOne.Let(slip.Symbol(warnANSIKey), nil)
		theOne.prompt = "* "
	}
	theOne.Let(slip.Symbol(form1Key), nil)
	theOne.Let(slip.Symbol(form2Key), nil)
	theOne.Let(slip.Symbol(form3Key), nil)
	theOne.Let(slip.Symbol(value1Key), nil)
	theOne.Let(slip.Symbol(value2Key), nil)
	theOne.Let(slip.Symbol(value3Key), nil)

	slip.SetHook = setHook
	slip.UnsetHook = unsetHook
	slip.DefunHook = defunHook

	slip.DefConstant(slip.Symbol("*repl*"), &theOne, "The REPL.")
}

type loop struct {
	flavors.Instance
	line  []byte
	buf   []byte
	depth int

	form1 slip.Object
	form2 slip.Object
	form3 slip.Object

	value1 slip.Object
	value2 slip.Object
	value3 slip.Object

	prompt string
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
	configFilename = filepath.Join(dir, "config.lisp")
	historyFilename = filepath.Join(dir, "history")

	os.MkdirAll(dir, 0755)
	var buf []byte
	if buf, err = os.ReadFile(configFilename); err == nil {
		scope := slip.NewScope()
		code := slip.Read(buf)
		code.Compile()
		code.Eval(scope)
	} else if os.IsNotExist(err) {
		if err = os.WriteFile(configFilename, []byte(configHeader), 0666); err != nil {
			panic(err)
		}
	} else {
		panic(err)
	}
}

// Scope returns the REPL scope.
func Scope() *slip.Scope {
	return &theOne.Instance.Scope
}

func Run() {
	defer func() {
		_ = recover()
		_, _ = theOne.Get(slip.Symbol(stdOutput)).(io.Writer).Write([]byte("\nBye\n"))
	}()
	for {
		theOne.process()
	}
}

func (r *loop) Let(sym slip.Symbol, value slip.Object) {
	r.Instance.Scope.Let(sym, value)
	modifiedVars[string(sym)] = false
	updateConfigFile()
}

// Receive a method invocation from the send function.
func (r *loop) Receive(message string, args slip.List, depth int) slip.Object {
	message = strings.ToLower(message)
	switch message {
	case ":set-prompt":
		if str, ok := args[0].(slip.String); ok {
			r.prompt = string(str)
			modifiedVars["prompt"] = false
		} else {
			panic("send set-prompt to *repl* expects a string argument")
		}
	default:
		return r.Instance.Receive(message, args, depth)
	}
	updateConfigFile()
	return args[0]
}

func (r *loop) reset() {
	r.buf = r.buf[:0]
	r.depth = 0
}

func (r *loop) process() {
	defer func() {
		rec := recover()
		if rec == nil {
			r.reset()
			return
		}
		var (
			prefix string
			suffix string
		)
		if r.Get(slip.Symbol(printANSI)) != nil {
			if ansi := r.Get(slip.Symbol(warnANSIKey)); ansi != nil {
				s, _ := ansi.(slip.String)
				prefix = string(s)
				suffix = "\x1b[m"
			}
		}
		switch tr := rec.(type) {
		case *slip.Partial:
			r.depth = tr.Depth
		case *slip.Panic:
			var buf []byte
			buf = append(buf, prefix...)
			buf = append(buf, tr.Bytes()...)
			buf = append(buf, suffix...)
			_, _ = r.Get(slip.Symbol(stdOutput)).(io.Writer).Write(buf)
			r.reset()
			debug.PrintStack()
		case error:
			if errors.Is(tr, io.EOF) {
				panic(nil) // exits the REPL loop
			}
			debug.PrintStack()
			fmt.Fprintf(r.Get(slip.Symbol(stdOutput)).(io.Writer), "%s## %v%s\n", prefix, tr, suffix)
			r.reset()
		default:
			debug.PrintStack()
			fmt.Fprintf(r.Get(slip.Symbol(stdOutput)).(io.Writer), "%s## %v%s\n", prefix, tr, suffix)
			r.reset()
		}
	}()
	r.read()
	code := slip.Read(r.buf)
	for _, obj := range code {
		var skipWrite bool

		r.form3 = r.form2
		r.form2 = r.form1
		r.form1 = obj

		r.value3 = r.value2
		r.value2 = r.value1
		r.value1 = obj.Eval(&r.Instance.Scope, 0)
		if r.value1 == slip.Novalue {
			r.value1 = nil
			skipWrite = true
		}
		// Eval was successful.
		writeToHistory()

		r.Set(slip.Symbol(form1Key), r.form1)
		r.Set(slip.Symbol(form2Key), r.form2)
		r.Set(slip.Symbol(form3Key), r.form3)

		r.Set(slip.Symbol(value1Key), r.value1)
		r.Set(slip.Symbol(value2Key), r.value2)
		r.Set(slip.Symbol(value3Key), r.value3)

		if !skipWrite {
			fmt.Fprintf(r.Get(slip.Symbol(stdOutput)).(io.Writer), "%s\n", slip.ObjectString(r.value1))
		}
	}
}

func (r *loop) read() {
	var prefix slip.String
	var suffix string

	// TBD how to set the prompt for other levels with bold or color
	//   maybe just ansi code for before and also prompt character
	//   or secondary prompt with %d or a list of pre and post with number in the middle
	//     %d option allows for %02d or maybe format function
	if 0 < len(r.buf) {
		fmt.Fprintf(r.Get(slip.Symbol(stdOutput)).(io.Writer), "%s%d] %s", string(prefix), r.depth, suffix)
	} else {
		fmt.Fprintf(r.Get(slip.Symbol(stdOutput)).(io.Writer), "%s", theOne.prompt)
	}
	for {
		n, err := r.Get(slip.Symbol(stdInput)).(io.Reader).Read(r.line)
		if err != nil {
			panic(err)
		}
		r.buf = append(r.buf, r.line[:n]...)
		if 0 < len(r.buf) && r.buf[len(r.buf)-1] == '\n' {
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
		if modifiedVars[key] {
			value := slip.UserPkg.JustGet(key)
			b = fmt.Appendf(b, "(setq %s %s)\n", key, value)
		} else {
			switch key {
			case "prompt":
				b = fmt.Appendf(b, "(send *repl* :set-prompt %q)\n", theOne.prompt)
			case "tbd":
				// TBD others
			}
		}
	}
	if err := os.WriteFile(configFilename, b, 0666); err != nil {
		panic(err)
	}
}

func writeToHistory() {
	// TBD write r.buf to history
}

func setHook(p *slip.Package, key string) {
	if strings.HasPrefix(key, "*print-") || key == "*bag-time-format*" || key == "*bag-time-wrap*" {
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
