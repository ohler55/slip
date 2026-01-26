// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"fmt"

	"github.com/ohler55/slip"
)

// Verbose flags for debugging output
var (
	VerboseWire     bool
	VerboseDispatch bool
	VerboseEval     bool
	VerboseColor    bool
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
	prefix := "[slynk:wire]"
	if VerboseColor {
		fmt.Printf("%s%s%s %s %s\n", colorCyan, prefix, colorReset, direction, slip.ObjectString(msg))
	} else {
		fmt.Printf("%s %s %s\n", prefix, direction, slip.ObjectString(msg))
	}
}

// LogDispatch logs handler dispatch if VerboseDispatch is enabled.
func LogDispatch(handler string, args slip.List) {
	if !VerboseDispatch {
		return
	}
	prefix := "[slynk:dispatch]"
	if VerboseColor {
		fmt.Printf("%s%s%s %s %s\n", colorYellow, prefix, colorReset, handler, slip.ObjectString(args))
	} else {
		fmt.Printf("%s %s %s\n", prefix, handler, slip.ObjectString(args))
	}
}

// LogEval logs evaluation if VerboseEval is enabled.
func LogEval(expr slip.Object, result slip.Object) {
	if !VerboseEval {
		return
	}
	prefix := "[slynk:eval]"
	if VerboseColor {
		fmt.Printf("%s%s%s %s => %s\n", colorGreen, prefix, colorReset, slip.ObjectString(expr), slip.ObjectString(result))
	} else {
		fmt.Printf("%s %s => %s\n", prefix, slip.ObjectString(expr), slip.ObjectString(result))
	}
}

// LogError logs errors with optional color.
func LogError(format string, args ...any) {
	prefix := "[slynk:error]"
	msg := fmt.Sprintf(format, args...)
	if VerboseColor {
		fmt.Printf("%s%s%s %s\n", colorRed, prefix, colorReset, msg)
	} else {
		fmt.Printf("%s %s\n", prefix, msg)
	}
}

func init() {
	defSlynkVerbose()
}

// defSlynkVerbose defines the slynk-verbose function.
func defSlynkVerbose() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SlynkVerbose{Function: slip.Function{Name: "slynk-verbose", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "slynk-verbose",
			Args: []*slip.DocArg{
				{Name: "&key"},
				{Name: "wire", Type: "boolean", Text: "log wire protocol messages."},
				{Name: "dispatch", Type: "boolean", Text: "log handler dispatch."},
				{Name: "eval", Type: "boolean", Text: "log evaluation."},
				{Name: "color", Type: "boolean", Text: "use ANSI colors in output."},
			},
			Return: "list",
			Text: `__slynk-verbose__ controls debug logging for the Slynk server.
With no arguments, returns the current verbose settings.
With keyword arguments, sets the specified flags.

Categories:
  :wire     - Raw S-expressions sent/received over the socket
  :dispatch - Handler name and arguments for each RPC call
  :eval     - SLIP expressions being evaluated and results
  :color    - Enable ANSI colors (cyan=wire, yellow=dispatch, green=eval)`,
			Examples: []string{
				`(slynk-verbose :wire t :dispatch t :color t)`,
				`(slynk-verbose) => (:wire t :dispatch t :eval nil :color t)`,
			},
		}, &Pkg)
}

// SlynkVerbose represents the slynk-verbose function.
type SlynkVerbose struct {
	slip.Function
}

// Call implements the slynk-verbose function.
func (f *SlynkVerbose) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
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
