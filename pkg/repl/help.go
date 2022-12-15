// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"io"
	"strings"

	"github.com/ohler55/slip"
)

const (
	helpTop = `
__SLIce Processing (SLIP) is an ANSI Common LISP written in go__


SLIP allows for extensions written in go. It deviates from most other LISP
implementations in that it represents lists as go slices. This sometimes
creates compatibility issues when manipulating a CONS directly but in most
case SLIP is compatible with Common LISP.


For the index for the REPL help type _(help 'index)_.


This REPL can be configured using LISP functions and by setting global
variables such as _*repl-prompt*_. Changes are saved to a configuration file
which by default is in __~/.slip/config.lisp__ which loaded on startup to preserve
settings on restart. For more informaion on on configuration type _(help
'config)_.


The SLIP REPL includes an enhanced editor which includes multi-line editing,
tab completion, and parenthesis matching. For more information on editing type
_(help 'edit)_.
`
	helpIndex = `__SLIP REPL Help Index__


  • __index__ - this page
  • __config__ - describes how to configure the REPL
  • __edit__ - describes the editor and key bindings
  • __history__ - explains how to use command history
  • __useful__ - describes some useful functions
`
	helpConfig = `__SLIP REPL Configuration__


A configuration direction can be provided on startup of the REPL. If one is
not provided then __~/.slip__ is used unless the -c option with an empty
string or - is specified. In the configuration directory there can be three
files:


  • __config.lisp__ contains _setq_ function calls that set variables on
    startup of the REPL. This file over-written by the REPL as _setq_
    functions are invoked on REPL and _*print-xxx*_ variables.

  • __custom.lisp__ is loaded on startup. It is where customiations can be
    made.

  • __history__ contains a history of forms evaluated in the REPL.
`
	helpEdit = `__SLIP REPL Editing__


TBD keys bindings, completions, colorization
`
	helpHistory = `__SLIP REPL History__


The SLIP REPL maintains a limited history of forms entered in the REPL. The
length of the history is limited by the _*repl-history-limit*_ variable. The
history is stored in the configuration directory in a file called "history".
`
	helpUseful = `
__Useful SLIP Functions__


Two of the most useful functions in SLIP or for that matter Common LISP are
_apropos_ and _describe_.


The _apropos_ function will list all functions and variables whose name
includes a provided string. For example _(apropos "repl")_ will print all the
REPL functions and variables that include the string "repl".


The _describe_ function provides a description of the provided symbol. The
description includes the function documentation, arguments, and usually
examples. For variables the documentation, type, and value are printed.
`
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Help{Function: slip.Function{Name: "help", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "help",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The _object_ to check whether it is a _bag-path_ or not.",
				},
			},
			Text: `__help__ returns _t_ if _object- is a _bag-path_ and _nil_ otherwise.`,
			Examples: []string{
				`(help) => nil ;; print help text`,
			},
		}, &Pkg)
}

// Help represents the help function.
type Help struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Help) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if 1 < len(args) {
		slip.PanicArgCount(f, 0, 1)
	}
	w := slip.CLPkg.JustGet(stdOutput).(io.Writer)
	text := helpTop
	if 0 < len(args) {
		var topic string
		switch ta := args[0].(type) {
		case slip.Symbol:
			topic = string(ta)
		case slip.String:
			topic = string(ta)
		default:
			slip.PanicType("topic", ta, "string", "symbol")
		}
		switch strings.ToLower(topic) {
		case "index":
			text = helpIndex
		case "config", "configuration":
			text = helpConfig
		case "edit", "editing":
			text = helpEdit
		case "history":
			text = helpHistory
		case "useful":
			text = helpUseful
		default:
			fmt.Fprintf(w, "%s is not a supported topic.\n\n", topic)
			text = helpIndex
		}
	}
	_, _ = w.Write([]byte{'\n'})
	_, _ = w.Write(slip.AppendDoc(nil, text, 0, 80, s.Get(slip.Symbol("*print-ansi*")) != nil))
	_, _ = w.Write([]byte{'\n', '\n'})

	return slip.Novalue
}
