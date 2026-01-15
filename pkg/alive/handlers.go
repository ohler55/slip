// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive

import (
	"strings"
	"sync"
)

// Handler processes an LSP request and returns a result or error.
// The params argument contains the decoded JSON params from the request.
// Returns (result, nil) for success or (nil, error) for failure.
type Handler func(c *Connection, params interface{}) (result interface{}, err error)

var (
	handlers   = make(map[string]Handler)
	handlersMu sync.RWMutex
)

// RegisterHandler registers a handler for the given method name.
// Method names are case-sensitive and should match LSP conventions:
// - Standard LSP: "initialize", "textDocument/completion", etc.
// - Alive custom: "$/alive/eval", "$/alive/listPackages", etc.
func RegisterHandler(name string, h Handler) {
	handlersMu.Lock()
	defer handlersMu.Unlock()

	handlers[name] = h

	// Also register lowercase version for case-insensitive lookup
	lower := strings.ToLower(name)
	if lower != name {
		handlers[lower] = h
	}
}

// GetHandler returns the handler for the given method name.
// Returns nil if no handler is registered.
func GetHandler(name string) Handler {
	handlersMu.RLock()
	defer handlersMu.RUnlock()

	if h, ok := handlers[name]; ok {
		return h
	}

	// Try lowercase
	if h, ok := handlers[strings.ToLower(name)]; ok {
		return h
	}

	return nil
}

// ListHandlers returns the names of all registered handlers.
func ListHandlers() []string {
	handlersMu.RLock()
	defer handlersMu.RUnlock()

	// Use a map to deduplicate (since we register both cases)
	seen := make(map[string]bool)
	var names []string

	for name := range handlers {
		// Only include the original case version
		if !seen[strings.ToLower(name)] {
			names = append(names, name)
			seen[strings.ToLower(name)] = true
		}
	}

	return names
}
