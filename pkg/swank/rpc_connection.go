// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"os"
	"runtime"

	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("swank:connection-info", handleConnectionInfo)
	RegisterHandler("swank:create-repl", handleCreateRepl)
	RegisterHandler("swank:buffer-first-change", handleBufferFirstChange)
}

// handleConnectionInfo returns information about the Lisp implementation.
// Response format: plist with :pid, :lisp-implementation, :machine, :features, etc.
func handleConnectionInfo(c *Connection, args slip.List) slip.Object {
	hostname, _ := os.Hostname()

	return slip.List{
		slip.Symbol(":pid"), slip.Fixnum(os.Getpid()),
		slip.Symbol(":style"), slip.Symbol(":spawn"),
		slip.Symbol(":encoding"), slip.List{
			slip.Symbol(":coding-systems"),
			slip.List{slip.String("utf-8-unix")},
		},
		slip.Symbol(":lisp-implementation"), slip.List{
			slip.Symbol(":type"), slip.String("SLIP"),
			slip.Symbol(":name"), slip.String("slip"),
			slip.Symbol(":version"), slip.String("1.0.0"),
			slip.Symbol(":program"), slip.String(os.Args[0]),
		},
		slip.Symbol(":machine"), slip.List{
			slip.Symbol(":instance"), slip.String(hostname),
			slip.Symbol(":type"), slip.String(runtime.GOARCH),
			slip.Symbol(":version"), slip.String(runtime.GOOS),
		},
		slip.Symbol(":features"), getFeatures(),
		slip.Symbol(":modules"), slip.List{},
		slip.Symbol(":package"), slip.List{
			slip.Symbol(":name"), slip.String(c.currentPkg.Name),
			slip.Symbol(":prompt"), slip.String(c.currentPkg.Name),
		},
		slip.Symbol(":version"), slip.String("2.29"), // Swank protocol version
	}
}

// getFeatures returns a list of SLIP features.
func getFeatures() slip.List {
	features := slip.List{
		slip.Symbol(":slip"),
		slip.Symbol(":common-lisp"),
		slip.Symbol(":go"),
	}

	// Add platform-specific features
	switch runtime.GOOS {
	case "darwin":
		features = append(features, slip.Symbol(":darwin"), slip.Symbol(":unix"))
	case "linux":
		features = append(features, slip.Symbol(":linux"), slip.Symbol(":unix"))
	case "windows":
		features = append(features, slip.Symbol(":windows"))
	default:
		features = append(features, slip.Symbol(":unix"))
	}

	switch runtime.GOARCH {
	case "amd64":
		features = append(features, slip.Symbol(":x86-64"), slip.Symbol(":64-bit"))
	case "arm64":
		features = append(features, slip.Symbol(":arm64"), slip.Symbol(":64-bit"))
	case "386":
		features = append(features, slip.Symbol(":x86"), slip.Symbol(":32-bit"))
	}

	return features
}

// handleCreateRepl initializes a REPL session.
// Response format: (package-name prompt-string)
func handleCreateRepl(c *Connection, args slip.List) slip.Object {
	// Initialize history variables in scope
	c.scope.Let(slip.Symbol("*"), nil)
	c.scope.Let(slip.Symbol("**"), nil)
	c.scope.Let(slip.Symbol("***"), nil)
	c.scope.Let(slip.Symbol("+"), nil)
	c.scope.Let(slip.Symbol("++"), nil)
	c.scope.Let(slip.Symbol("+++"), nil)
	c.scope.Let(slip.Symbol("/"), nil)
	c.scope.Let(slip.Symbol("//"), nil)
	c.scope.Let(slip.Symbol("///"), nil)

	return slip.List{
		slip.String(c.currentPkg.Name),
		slip.String(c.currentPkg.Name),
	}
}

// handleBufferFirstChange is called when a buffer is first modified.
// We just acknowledge it.
func handleBufferFirstChange(c *Connection, args slip.List) slip.Object {
	return nil
}
