// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("swank:listener-eval", handleListenerEval)
	RegisterHandler("swank-repl:listener-eval", handleListenerEval) // SLIME uses this variant
	RegisterHandler("swank:interactive-eval", handleInteractiveEval)
	RegisterHandler("swank:interactive-eval-region", handleInteractiveEvalRegion)
	RegisterHandler("swank:eval-and-grab-output", handleEvalAndGrabOutput)
	RegisterHandler("swank:pprint-eval", handlePprintEval)
}

// handleListenerEval evaluates code in the REPL context.
// This is the main evaluation entry point for the SLIME REPL.
func handleListenerEval(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return nil
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return nil
	}

	return evalWithCapture(c, string(source), true)
}

// handleInteractiveEval evaluates code from the minibuffer.
func handleInteractiveEval(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return nil
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return nil
	}

	result := evalWithCapture(c, string(source), false)

	// Return the result as a string for display
	return slip.String("=> " + slip.ObjectString(result))
}

// handleInteractiveEvalRegion evaluates a region of code.
func handleInteractiveEvalRegion(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return nil
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return nil
	}

	result := evalWithCapture(c, string(source), false)
	return slip.String("=> " + slip.ObjectString(result))
}

// handleEvalAndGrabOutput evaluates and returns both output and value.
// Response format: (output-string result-string)
func handleEvalAndGrabOutput(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.List{slip.String(""), slip.String("nil")}
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return slip.List{slip.String(""), slip.String("nil")}
	}

	result := evalWithCapture(c, string(source), false)

	// Get captured output
	output := c.outputStream.Flush()

	return slip.List{
		slip.String(output),
		slip.String(slip.ObjectString(result)),
	}
}

// handlePprintEval evaluates and pretty-prints the result.
func handlePprintEval(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return nil
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return nil
	}

	result := evalWithCapture(c, string(source), false)
	return slip.String(slip.ObjectString(result))
}

// evalWithCapture evaluates code with output capture and error handling.
// If sendOutput is true, output is sent to SLIME as :write-string messages.
func evalWithCapture(c *Connection, source string, sendOutput bool) (result slip.Object) {
	defer func() {
		if r := recover(); r != nil {
			if p, ok := r.(*slip.Panic); ok {
				if p.Message == "Keyboard interrupt" {
					panic(r) // propagate to handleEmacsRex goroutine recovery
				}
				errMsg := fmt.Sprintf("Error: %s\n", p.Message)
				if sendOutput {
					c.WriteString(errMsg)
				}
				result = nil
			} else {
				errMsg := fmt.Sprintf("Error: %v\n", r)
				if sendOutput {
					c.WriteString(errMsg)
				}
				result = nil
			}
		}
	}()

	// Parse the source
	code := slip.Read([]byte(source), c.scope)
	if len(code) == 0 {
		return nil
	}

	// Compile
	code.Compile()

	// Track the form for history
	var form slip.Object
	if len(code) > 0 {
		form = code[0]
	}

	// Bind standard output to the capture stream during evaluation so that
	// functions like princ (which read the Go-level global) write to the
	// connection stream rather than os.Stdout. Serialized via stdOutMu —
	// see output.go — because the global would otherwise race across
	// concurrent :emacs-rex goroutines.
	withStandardOutput(c.outputStream, func() {
		result = code.Eval(c.scope, nil)
	})

	// Log evaluation if verbose
	LogEval(form, result)

	// Flush any captured output
	if sendOutput {
		c.outputStream.FlushToSlime()
	}

	// Update history
	c.UpdateHistory(result, form)

	// Send result to SLIME
	if sendOutput {
		c.WriteResult(slip.ObjectString(result))
	}

	return result
}
