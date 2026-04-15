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
	VerboseColor    bool
	LogOutput       io.Writer = os.Stdout
)

// ANSI color codes (for black background)
const (
	colorReset  = "\x1b[0m"
	colorCyan   = "\x1b[36m" // wire
	colorYellow = "\x1b[33m" // dispatch
	colorGreen  = "\x1b[32m" // eval
	colorRed    = "\x1b[31m" // errors
)

// LogWire logs wire protocol messages if VerboseWire is enabled.
// direction should be "<-" for incoming or "->" for outgoing.
func LogWire(direction string, msg slip.Object) {
	if !VerboseWire {
		return
	}
	prefix := "[swank:wire]"
	if VerboseColor {
		fmt.Fprintf(LogOutput, "%s%s%s %s %s\n", colorCyan, prefix, colorReset, direction, slip.ObjectString(msg))
	} else {
		fmt.Fprintf(LogOutput, "%s %s %s\n", prefix, direction, slip.ObjectString(msg))
	}
}

// LogDispatch logs handler dispatch if VerboseDispatch is enabled.
func LogDispatch(handler string, args slip.List) {
	if !VerboseDispatch {
		return
	}
	prefix := "[swank:dispatch]"
	if VerboseColor {
		fmt.Fprintf(LogOutput, "%s%s%s %s %s\n", colorYellow, prefix, colorReset, handler, slip.ObjectString(args))
	} else {
		fmt.Fprintf(LogOutput, "%s %s %s\n", prefix, handler, slip.ObjectString(args))
	}
}

// LogEval logs evaluation if VerboseEval is enabled.
func LogEval(expr slip.Object, result slip.Object) {
	if !VerboseEval {
		return
	}
	prefix := "[swank:eval]"
	if VerboseColor {
		fmt.Fprintf(LogOutput, "%s%s%s %s => %s\n", colorGreen, prefix, colorReset, slip.ObjectString(expr), slip.ObjectString(result))
	} else {
		fmt.Fprintf(LogOutput, "%s %s => %s\n", prefix, slip.ObjectString(expr), slip.ObjectString(result))
	}
}

// LogError logs errors with optional color.
func LogError(format string, args ...any) {
	prefix := "[swank:error]"
	msg := fmt.Sprintf(format, args...)
	if VerboseColor {
		fmt.Fprintf(LogOutput, "%s%s%s %s\n", colorRed, prefix, colorReset, msg)
	} else {
		fmt.Fprintf(LogOutput, "%s %s\n", prefix, msg)
	}
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
				{Name: "color", Type: "boolean", Text: "use ANSI colors in output."},
			},
			Return: "list",
			Text: `__swank-verbose__ controls debug logging for the Swank server.
With no arguments, returns the current verbose settings.
With keyword arguments, sets the specified flags.

Categories:
  :wire     - Raw S-expressions sent/received over the socket
  :dispatch - Handler name and arguments for each RPC call
  :eval     - SLIP expressions being evaluated and results
  :color    - Enable ANSI colors (cyan=wire, yellow=dispatch, green=eval)`,
			Examples: []string{
				`(swank-verbose :wire t :dispatch t :color t)`,
				`(swank-verbose) => (:wire t :dispatch t :eval nil :color t)`,
			},
		}, &Pkg)
}

// SwankVerbose represents the swank-verbose function.
type SwankVerbose struct {
	slip.Function
}

// Call implements the swank-verbose function.
func (f *SwankVerbose) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 8)

	// If no args, return current state
	if len(args) == 0 {
		return slip.List{
			slip.Symbol(":wire"), boolToLisp(VerboseWire),
			slip.Symbol(":dispatch"), boolToLisp(VerboseDispatch),
			slip.Symbol(":eval"), boolToLisp(VerboseEval),
			slip.Symbol(":color"), boolToLisp(VerboseColor),
		}
	}

	// Parse keyword arguments
	for i := 0; i < len(args); i += 2 {
		key, ok := args[i].(slip.Symbol)
		if !ok || i+1 >= len(args) {
			slip.TypePanic(s, depth, "keyword", args[i], ":wire", ":dispatch", ":eval", ":color")
		}
		val := args[i+1] != nil

		switch key {
		case ":wire":
			VerboseWire = val
		case ":dispatch":
			VerboseDispatch = val
		case ":eval":
			VerboseEval = val
		case ":color":
			VerboseColor = val
		default:
			slip.TypePanic(s, depth, "keyword", args[i], ":wire", ":dispatch", ":eval", ":color")
		}
	}

	return slip.List{
		slip.Symbol(":wire"), boolToLisp(VerboseWire),
		slip.Symbol(":dispatch"), boolToLisp(VerboseDispatch),
		slip.Symbol(":eval"), boolToLisp(VerboseEval),
		slip.Symbol(":color"), boolToLisp(VerboseColor),
	}
}

func boolToLisp(b bool) slip.Object {
	if b {
		return slip.True
	}
	return nil
}
