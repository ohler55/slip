// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("swank:swank-toggle-trace", handleToggleTrace)
	RegisterHandler("swank:toggle-trace", handleToggleTrace)
	RegisterHandler("swank:untrace-all", handleUntraceAll)
}

// handleToggleTrace toggles tracing for a function.
// Args: (spec) where spec is a string like "append" or "(setf foo)"
// Response: string message indicating trace status
func handleToggleTrace(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.String("No function specified")
	}

	spec := ""
	switch v := args[0].(type) {
	case slip.String:
		spec = string(v)
	case slip.Symbol:
		spec = string(v)
	default:
		return slip.String(fmt.Sprintf("Invalid trace spec: %v", args[0]))
	}

	// Parse the spec - SLIME can send complex specs like "(setf foo)" or "(:defmethod ...)"
	// For SLIP, we support simple function names
	funcName := parseTraceSpec(spec)
	if funcName == "" {
		return slip.String(fmt.Sprintf("Cannot parse trace spec: %s", spec))
	}

	// Check if currently traced
	traced := isTraced(funcName)

	if traced {
		// Untrace it
		slip.Untrace(slip.List{slip.Symbol(funcName)})
		return slip.String(fmt.Sprintf("Untraced %s", strings.ToUpper(funcName)))
	}

	// Trace it
	slip.Trace(slip.List{slip.Symbol(funcName)})
	return slip.String(fmt.Sprintf("Tracing %s", strings.ToUpper(funcName)))
}

// handleUntraceAll removes all traces.
func handleUntraceAll(c *Connection, args slip.List) slip.Object {
	slip.Untrace(nil)
	return slip.String("Untraced all functions")
}

// parseTraceSpec extracts a function name from a SLIME trace spec.
// SLIME specs can be:
//   - "function-name" - simple function
//   - "(setf function-name)" - setf function
//   - "(:defmethod name ...)" - method (not fully supported)
//   - "(:defgeneric name)" - generic function
func parseTraceSpec(spec string) string {
	spec = strings.TrimSpace(spec)

	// Simple name
	if !strings.HasPrefix(spec, "(") {
		return strings.ToLower(spec)
	}

	// Strip outer parens
	if strings.HasPrefix(spec, "(") && strings.HasSuffix(spec, ")") {
		inner := strings.TrimSpace(spec[1 : len(spec)-1])

		// (setf name)
		if strings.HasPrefix(strings.ToLower(inner), "setf ") {
			parts := strings.Fields(inner)
			if len(parts) >= 2 {
				return "setf-" + strings.ToLower(parts[1])
			}
		}

		// (:defgeneric name) or (:defmethod name ...)
		if strings.HasPrefix(inner, ":") {
			parts := strings.Fields(inner)
			if len(parts) >= 2 {
				return strings.ToLower(parts[1])
			}
		}

		// Just a name in parens
		return strings.ToLower(inner)
	}

	return ""
}

// isTraced checks if a function is currently being traced.
// This accesses SLIP's trace state.
func isTraced(name string) bool {
	// Get list of traced functions
	traced := slip.Trace(nil)
	if traced == nil {
		return false
	}

	// If tracing all (returns (t)), check differently
	if len(traced) == 1 && traced[0] == slip.True {
		return true
	}

	// Check if name is in the list
	name = strings.ToLower(name)
	for _, t := range traced {
		if sym, ok := t.(slip.Symbol); ok {
			if strings.ToLower(string(sym)) == name {
				return true
			}
		}
	}

	return false
}
