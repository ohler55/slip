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
	"github.com/ohler55/slip/pkg/clos"
)

const (
	printANSI       = "*print-ansi*"
	stdInput        = "*standard-input*"
	stdOutput       = "*standard-output*"
	editorFlagsName = "*repl-editor-flags*"

	form1Key  = "+"
	form2Key  = "++"
	form3Key  = "+++"
	value1Key = "/"
	value2Key = "//"
	value3Key = "///"

	configHeader = ";;;; slip REPL configuration file. For help type: (help 'configuration)\n\n"
)

var (
	// Interactive is set to true when the REPL is interactive.
	Interactive bool

	// Trace reflects the value of tracing as set on startup. It does no
	// reflect the current trace state.
	Trace bool

	// DebugEditor if set before Run() is called will log keystrokes to
	// editor.log when using the editor.
	DebugEditor bool

	modifiedVars    = map[string]bool{}
	configFilename  = ""
	historyFilename = ""

	scope          slip.Scope
	prompt         string
	warnPrefix     string
	matchColor     string
	evalOnClose    bool
	externalEditor = ""
	editorFlags    slip.List

	form1 slip.Object
	form2 slip.Object
	form3 slip.Object

	value1 slip.Object
	value2 slip.Object
	value3 slip.Object

	replReader reader = &termReader{}

	stashLoadPath = slip.List{
		slip.String("~/.config/slip"),
		slip.String("~/.slip"),
		slip.String("."),
	}
	defaultStashName = "stash.lisp"

	sizer hasSize
	// TBD flag to not let Run() be called more than once before (repl-exit) is called
)

func init() {
	scope.SetSynchronized(false)
	if ev := os.Getenv("XDG_CONFIG_HOME"); 0 < len(ev) {
		stashLoadPath = append(slip.List{slip.String(ev)}, stashLoadPath...)
	}
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

	slip.AddSetHook("repl", setHook)
	slip.AddUnsetHook("repl", unsetHook)
	slip.AddDefunHook("repl", addHook)
	slip.AddClassHook("repl", addHook)
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
	defer func() {
		_ = slip.CurrentPackage.Set("*load-pathname*", nil)
		_ = slip.CurrentPackage.Set("*load-truename*", nil)
	}()
	if buf, err = os.ReadFile(cfgPath); err == nil {
		configFilename = "" // Turn off writing while evaluating config file.
		if Trace {
			fmt.Printf("Loading %q.\n", cfgPath)
		}
		code := slip.Read(buf, &scope)
		pathname := slip.String(filepath.Join(slip.WorkingDir, cfgPath))
		_ = slip.CurrentPackage.Set("*load-pathname*", pathname)
		_ = slip.CurrentPackage.Set("*load-truename*", pathname)
		code.Compile()
		code.Eval(&scope, nil) // TBD consider at load-verbose and load-print
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
		pathname := slip.String(filepath.Join(slip.WorkingDir, customPath))
		_ = slip.CurrentPackage.Set("*load-pathname*", pathname)
		_ = slip.CurrentPackage.Set("*load-truename*", pathname)
		if Trace {
			fmt.Printf("Loading %q.\n", pathname)
		}
		code := slip.Read(buf, &scope)
		code.Compile()
		code.Eval(&scope, nil) // TBD look at load-verbose and load-print
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
	TheHistory.SetLimit(1000) // initial value that the user can replace by setting *repl-history-limit*
	TheHistory.Load(historyFilename)
	initStash()

	Interactive = true
	if _, ok := replReader.(*termReader); ok {
		fmt.Println(`Currently using a basic terminal editor. For a more interactive editor enter
(setq *repl-editor* t).`)
	}
	replReader.initialize()
	for {
		process()
	}
}

// Stop and reset the terminal.
func Stop() {
	Interactive = false
	replReader.stop()
}

// ZeroMods resets the modified variables list.
func ZeroMods() {
	modifiedVars = map[string]bool{}
}

// FindConfigDir finds the preferred config directory.
func FindConfigDir() (dir string) {
	home, _ := os.UserHomeDir()
	if path := os.Getenv("XDG_CONFIG_HOME"); 0 < len(path) {
		dir = strings.Replace(path, "~", home, 1)
	} else {
		for _, path := range []string{"~/.config/slip", "~/.slip"} {
			fp := strings.Replace(path, "~", home, 1)
			if fi, err := os.Stat(fp); err == nil && fi.IsDir() {
				dir = fp
				break
			}
		}
		if len(dir) == 0 {
			dir = "~/.config/slip"
		}
	}
	if fp, err := filepath.Abs(filepath.Clean(dir)); err == nil {
		dir = fp
	}
	return
}

func initStash() {
	loadPaths := stashLoadPath
	if len(loadPaths) == 0 {
		loadPaths = append(loadPaths, slip.String("."))
	}
	name := defaultStashName
	if len(name) == 0 {
		name = "stash.lisp"
	}
	home, _ := os.UserHomeDir()
	for _, spath := range loadPaths {
		if path, ok := spath.(slip.String); ok {
			fp := strings.ReplaceAll(filepath.Join(string(path), name), "~", home)
			if fi, err := os.Stat(fp); err != nil || fi.IsDir() {
				continue
			}
			TheStash.LoadExpanded(fp)
			return
		}
	}
	if path, ok := loadPaths[0].(slip.String); ok {
		fp := strings.ReplaceAll(filepath.Join(string(path), name), "~", home)
		if err := os.WriteFile(fp, []byte{}, 0666); err != nil {
			fmt.Printf("failed to create a stash file at %s\n", fp)
			return
		}
		TheStash.LoadExpanded(fp)
	}
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
	top:
		switch tr := rec.(type) {
		case *slip.PartialPanic:
			replReader.setDepth(tr.Depth)
			return
		case *slip.Panic:
			if len(tr.Message) == 0 && tr.Fatal {
				panic("")
			}
			msg := tr.Error()
			if 0 < len(msg) {
				var buf []byte
				buf = append(buf, warnPrefix...)
				buf = tr.AppendFull(buf)
				buf = append(buf, suffix...)
				buf = append(buf, '\n')
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
			_, _ = fmt.Fprintf(scope.Get(slip.Symbol(stdOutput)).(io.Writer), "%s%s%s\n", warnPrefix, tr, suffix)
			panic("")
		case error:
			if errors.Is(tr, io.EOF) {
				panic(nil) // exits the REPL loop
			}
			_, _ = fmt.Fprintf(scope.Get(slip.Symbol(stdOutput)).(io.Writer), "%s%v%s\n", warnPrefix, tr, suffix)
			if scope.Get("*repl-debug*") != nil {
				debug.PrintStack()
			}
			reset()
		case *clos.StandardObject:
			rec = slip.WrapError(&scope, tr, "", nil)
			goto top
		default:
			_, _ = fmt.Fprintf(scope.Get(slip.Symbol(stdOutput)).(io.Writer), "%s%v%s\n", warnPrefix, tr, suffix)
			if scope.Get("*repl-debug*") != nil {
				debug.PrintStack()
			}
			reset()
		}
		replReader.addToHistory()
	}()
	buf := replReader.read()
	code := slip.Read(buf, &scope)
	// Enter was pressed and read was successful so save to history. If a
	// recoverable panic then that also adds to history in the defer.
	replReader.addToHistory()
	replReader.afterEval()
	for _, obj := range code {
		var skipWrite bool

		form3 = form2
		form2 = form1
		form1 = obj

		value3 = value2
		value2 = value1
		if obj == nil {
			value1 = nil
		} else {
			value1 = obj.Eval(&scope, 0)
			if value1 == slip.Novalue {
				value1 = nil
				skipWrite = true
			}
		}
		scope.Set(slip.Symbol(form1Key), form1)
		scope.Set(slip.Symbol(form2Key), form2)
		scope.Set(slip.Symbol(form3Key), form3)

		scope.Set(slip.Symbol(value1Key), value1)
		scope.Set(slip.Symbol(value2Key), value2)
		scope.Set(slip.Symbol(value3Key), value3)

		if !skipWrite {
			_, _ = fmt.Fprintf(scope.Get(slip.Symbol(stdOutput)).(io.Writer), "%s\n", slip.ObjectString(value1))
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
		if list, ok := value.(slip.List); ok && 0 < len(list) {
			b = append(b, '\'')
			b = p.Append(b, value, 0)
		} else {
			b = p.Append(b, value, 0)
		}
		b = append(b, ")\n"...)
	}
	if err := os.WriteFile(configFilename, b, 0666); err != nil {
		panic(err)
	}
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

func getEditorFlags() slip.Object {
	return editorFlags
}

func setEditorFlags(value slip.Object) {
	switch list := value.(type) {
	case nil:
		editorFlags = slip.List{}
	case slip.List:
		editorFlags = list
	default:
		panic("*repl-editor-flags* must be a list of strings")
	}
}

func getExternalEditor() slip.Object {
	return slip.String(externalEditor)
}

func setExternalEditor(value slip.Object) {
	if str, ok := value.(slip.String); ok {
		externalEditor = string(str)
	} else {
		panic("*repl-external-editor* must be a string")
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

func getInteractive() slip.Object {
	if Interactive {
		return slip.True
	}
	return nil
}

func setInteractive(_ slip.Object) {
	panic("*repl-interactive* is a read only variable")
}

func getStashLoadPath() slip.Object {
	return stashLoadPath
}

func setStashLoadPath(value slip.Object) {
	switch list := value.(type) {
	case nil:
		stashLoadPath = slip.List{}
	case slip.List:
		stashLoadPath = list
	default:
		panic("*stash-load-path* must be a list of strings")
	}
}

func getDefaultStashName() (name slip.Object) {
	if 0 < len(defaultStashName) {
		name = slip.String(defaultStashName)
	}
	return
}

func setDefaultStashName(value slip.Object) {
	switch tv := value.(type) {
	case nil:
		defaultStashName = ""
	case slip.String:
		defaultStashName = string(tv)
	case slip.Symbol:
		defaultStashName = string(tv)
	default:
		panic("*default-stash-name* must be a string or nil")
	}
}
