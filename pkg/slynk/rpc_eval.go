// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	// Standard evaluation handlers
	RegisterHandler("slynk:listener-eval", handleListenerEval)
	RegisterHandler("slynk:interactive-eval", handleInteractiveEval)
	RegisterHandler("slynk:interactive-eval-region", handleInteractiveEvalRegion)
	RegisterHandler("slynk:eval-and-grab-output", handleEvalAndGrabOutput)
	RegisterHandler("slynk:pprint-eval", handlePPrintEval)
}

// handleListenerEval evaluates code in the REPL context.
func handleListenerEval(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return nil
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return nil
	}

	ch := c.DefaultChannel()
	return evalInScope(c, ch, string(source), true)
}

// handleInteractiveEval evaluates a single expression (minibuffer).
func handleInteractiveEval(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return nil
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return nil
	}

	ch := c.DefaultChannel()
	result := evalInScope(c, ch, string(source), false)
	if result != nil {
		return slip.String(slip.ObjectString(result))
	}
	return slip.String("nil")
}

// handleInteractiveEvalRegion evaluates a region of code.
func handleInteractiveEvalRegion(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return nil
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return nil
	}

	ch := c.DefaultChannel()
	result := evalInScope(c, ch, string(source), false)
	if result != nil {
		return slip.String(slip.ObjectString(result))
	}
	return slip.String("nil")
}

// handleEvalAndGrabOutput evaluates and returns both output and value.
func handleEvalAndGrabOutput(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return slip.List{slip.String(""), slip.String("nil")}
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return slip.List{slip.String(""), slip.String("nil")}
	}

	ch := c.DefaultChannel()

	// Clear output buffer before eval
	c.outputStream.Flush()

	result := evalInScope(c, ch, string(source), false)

	// Grab any output that was produced
	output := c.outputStream.Flush()

	var resultStr string
	if result != nil {
		resultStr = slip.ObjectString(result)
	} else {
		resultStr = "nil"
	}

	return slip.List{slip.String(output), slip.String(resultStr)}
}

// handlePPrintEval evaluates and pretty-prints the result.
func handlePPrintEval(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return slip.String("nil")
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return slip.String("nil")
	}

	ch := c.DefaultChannel()
	result := evalInScope(c, ch, string(source), false)

	// Pretty print - for now just use ObjectString
	if result != nil {
		return slip.String(slip.ObjectString(result))
	}
	return slip.String("nil")
}

// evalInScope evaluates code in a channel's scope with output capture.
func evalInScope(c *Connection, ch *Channel, source string, updateHistory bool) (result slip.Object) {
	defer func() {
		if r := recover(); r != nil {
			if p, ok := r.(*slip.Panic); ok {
				c.WriteString(fmt.Sprintf("Error: %s\n", p.Message))
			} else {
				c.WriteString(fmt.Sprintf("Error: %v\n", r))
			}
			result = nil
		}
	}()

	// Parse the source
	source = strings.TrimSpace(source)
	if source == "" {
		return nil
	}

	code := slip.Read([]byte(source), ch.Scope())
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

	// Evaluate
	result = code.Eval(ch.Scope(), nil)

	// Log evaluation if verbose
	LogEval(form, result)

	// Update history if requested
	if updateHistory {
		ch.UpdateREPLHistory(result, form)
	}

	// Flush any captured output
	c.outputStream.FlushToSly()

	// Send result display
	if result != nil {
		c.WriteResult(slip.ObjectString(result))
	} else {
		c.WriteResult("nil")
	}

	return result
}
