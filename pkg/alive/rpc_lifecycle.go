// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive

import (
	"os"
	"runtime"
)

func init() {
	RegisterHandler("initialize", handleInitialize)
	RegisterHandler("shutdown", handleShutdown)
}

// handleInitialize handles the LSP initialize request.
// Returns server capabilities.
func handleInitialize(c *Connection, params interface{}) (interface{}, error) {
	c.initialized = true

	return map[string]interface{}{
		"capabilities": map[string]interface{}{
			"completionProvider": map[string]interface{}{
				"triggerCharacters": []string{":", "(", " "},
				"resolveProvider":   false,
			},
			"hoverProvider": true,
			"textDocumentSync": map[string]interface{}{
				"openClose": true,
				"change":    1, // Full sync
				"save": map[string]interface{}{
					"includeText": true,
				},
			},
		},
		"serverInfo": map[string]interface{}{
			"name":    "SLIP Alive Server",
			"version": "1.0.0",
		},
		"machine": map[string]interface{}{
			"type":    runtime.GOARCH,
			"version": runtime.GOOS,
		},
		"pid": os.Getpid(),
	}, nil
}

// handleShutdown handles the LSP shutdown request.
// Prepares for exit but does not terminate.
func handleShutdown(c *Connection, params interface{}) (interface{}, error) {
	c.shutdownReq = true
	return nil, nil
}
