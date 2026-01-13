// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"strings"
	"sync"

	"github.com/ohler55/slip"
)

// Handler is a function that handles an RPC call from SLY.
// It receives the connection and the arguments from the RPC form.
type Handler func(c *Connection, args slip.List) slip.Object

var (
	handlers   = make(map[string]Handler)
	handlersMu sync.RWMutex
)

// RegisterHandler registers an RPC handler for the given name.
// Handler names are case-insensitive and can be in either
// "slynk:function-name" or "function-name" format.
// Also registers swank: aliases for compatibility.
func RegisterHandler(name string, h Handler) {
	handlersMu.Lock()
	defer handlersMu.Unlock()

	// Normalize to lowercase
	name = strings.ToLower(name)
	handlers[name] = h

	// Also register without the "slynk:" prefix if present
	if strings.HasPrefix(name, "slynk:") {
		shortName := strings.TrimPrefix(name, "slynk:")
		handlers[shortName] = h
		// Also register swank: version for compatibility
		handlers["swank:"+shortName] = h
	}
}

// GetHandler returns the handler for the given name, or nil if not found.
func GetHandler(name string) Handler {
	handlersMu.RLock()
	defer handlersMu.RUnlock()

	name = strings.ToLower(name)
	if h, ok := handlers[name]; ok {
		return h
	}

	// Try with "slynk:" prefix
	if !strings.HasPrefix(name, "slynk:") && !strings.HasPrefix(name, "swank:") {
		if h, ok := handlers["slynk:"+name]; ok {
			return h
		}
	}

	// Try converting swank: to slynk:
	if strings.HasPrefix(name, "swank:") {
		slynkName := "slynk:" + strings.TrimPrefix(name, "swank:")
		if h, ok := handlers[slynkName]; ok {
			return h
		}
	}

	return nil
}

// ListHandlers returns a list of all registered handler names.
func ListHandlers() []string {
	handlersMu.RLock()
	defer handlersMu.RUnlock()

	names := make([]string, 0, len(handlers))
	for name := range handlers {
		names = append(names, name)
	}
	return names
}
