// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"fmt"
	"io"
	"os"

	"github.com/ohler55/slip"
)

// Verbose flags for debugging output
var (
	VerboseWire     bool
	VerboseDispatch bool
	VerboseEval     bool
	LogOutput       io.Writer = os.Stdout
)

// LogWire logs wire protocol messages if VerboseWire is enabled.
// direction should be "<-" for incoming or "->" for outgoing.
func LogWire(direction string, msg slip.Object) {
	if !VerboseWire {
		return
	}
	fmt.Fprintf(LogOutput, "[swank:wire] %s %s\n", direction, slip.ObjectString(msg))
}

// LogDispatch logs handler dispatch if VerboseDispatch is enabled.
func LogDispatch(handler string, args slip.List) {
	if !VerboseDispatch {
		return
	}
	fmt.Fprintf(LogOutput, "[swank:dispatch] %s %s\n", handler, slip.ObjectString(args))
}

// LogEval logs evaluation if VerboseEval is enabled.
func LogEval(expr slip.Object, result slip.Object) {
	if !VerboseEval {
		return
	}
	fmt.Fprintf(LogOutput, "[swank:eval] %s => %s\n", slip.ObjectString(expr), slip.ObjectString(result))
}

// LogError logs errors.
func LogError(format string, args ...any) {
	fmt.Fprintf(LogOutput, "[swank:error] %s\n", fmt.Sprintf(format, args...))
}

func init() {
	defSwankVerbose()
}

// defSwankVerbose defines the swank-verbose function.
func defSwankVerbose() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SwankVerbose{Function: slip.Function{Name: "swank-verbose", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "swank-verbose",
			Args: []*slip.DocArg{
				{Name: "&key"},
				{Name: "wire", Type: "boolean", Text: "log wire protocol messages."},
				{Name: "dispatch", Type: "boolean", Text: "log handler dispatch."},
				{Name: "eval", Type: "boolean", Text: "log evaluation."},
			},
			Return: "list",
			Text: `__swank-verbose__ controls debug logging for the Swank server.
With no arguments, returns the current verbose settings.
With keyword arguments, sets the specified flags.

Categories:
  :wire     - Raw S-expressions sent/received over the socket
  :dispatch - Handler name and arguments for each RPC call
  :eval     - SLIP expressions being evaluated and results`,
			Examples: []string{
				`(swank-verbose :wire t :dispatch t)`,
				`(swank-verbose) => (:wire t :dispatch t :eval nil)`,
			},
		}, &Pkg)
}

// SwankVerbose represents the swank-verbose function.
type SwankVerbose struct {
	slip.Function
}

// Call implements the swank-verbose function.
func (f *SwankVerbose) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 6)

	// If no args, return current state
	if len(args) == 0 {
		return slip.List{
			slip.Symbol(":wire"), boolToLisp(VerboseWire),
			slip.Symbol(":dispatch"), boolToLisp(VerboseDispatch),
			slip.Symbol(":eval"), boolToLisp(VerboseEval),
		}
	}

	// Parse keyword arguments
	for i := 0; i < len(args); i += 2 {
		key, ok := args[i].(slip.Symbol)
		if !ok || i+1 >= len(args) {
			slip.TypePanic(s, depth, "keyword", args[i], ":wire", ":dispatch", ":eval")
		}
		val := args[i+1] != nil

		switch key {
		case ":wire":
			VerboseWire = val
		case ":dispatch":
			VerboseDispatch = val
		case ":eval":
			VerboseEval = val
		default:
			slip.TypePanic(s, depth, "keyword", args[i], ":wire", ":dispatch", ":eval")
		}
	}

	return slip.List{
		slip.Symbol(":wire"), boolToLisp(VerboseWire),
		slip.Symbol(":dispatch"), boolToLisp(VerboseDispatch),
		slip.Symbol(":eval"), boolToLisp(VerboseEval),
	}
}

func boolToLisp(b bool) slip.Object {
	if b {
		return slip.True
	}
	return nil
}
