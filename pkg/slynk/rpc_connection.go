// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"fmt"
	"os"
	"runtime"

	"github.com/ohler55/slip"
)

func init() {
	// Connection info - returns server capabilities
	RegisterHandler("slynk:connection-info", handleConnectionInfo)

	// Create MREPL channel - main entry point for multiple REPLs
	RegisterHandler("slynk:create-mrepl", handleCreateMREPL)

	// Ping - heartbeat
	RegisterHandler("slynk:ping", handlePing)
}

// handleConnectionInfo returns information about the Slynk server.
func handleConnectionInfo(c *Connection, args slip.List) slip.Object {
	hostname, _ := os.Hostname()
	pid := os.Getpid()

	// SLY expects specific features for mrepl and other functionality
	features := slip.List{
		slip.Symbol("slynk"),
		slip.Symbol("mrepl"),
		slip.Symbol("fancy-inspector"),
		slip.Symbol("package-local-nicknames"),
	}

	return slip.List{
		slip.Symbol(":pid"), slip.Fixnum(pid),
		slip.Symbol(":style"), slip.Symbol(":spawn"),
		slip.Symbol(":encoding"), slip.List{
			slip.Symbol(":coding-systems"), slip.List{slip.String("utf-8")},
		},
		slip.Symbol(":lisp-implementation"), slip.List{
			slip.Symbol(":type"), slip.String("SLIP"),
			slip.Symbol(":name"), slip.String("slip"),
			slip.Symbol(":version"), slip.String("1.0.0"),
			slip.Symbol(":program"), slip.String("slip"),
		},
		slip.Symbol(":machine"), slip.List{
			slip.Symbol(":instance"), slip.String(hostname),
			slip.Symbol(":type"), slip.String(runtime.GOARCH),
			slip.Symbol(":version"), slip.String(runtime.GOOS),
		},
		slip.Symbol(":features"), features,
		slip.Symbol(":modules"), slip.List{},
		slip.Symbol(":package"), slip.List{
			slip.Symbol(":name"), slip.String("cl-user"),
			slip.Symbol(":prompt"), slip.String("cl-user"),
		},
		slip.Symbol(":version"), slip.String("2.29"), // Slynk protocol version
	}
}

// handleCreateMREPL creates a new MREPL channel.
// Returns: (channel-id channel-name)
func handleCreateMREPL(c *Connection, args slip.List) slip.Object {
	// Create a new channel for this REPL
	ch := c.CreateChannel("mrepl")

	// Notify SLY about the new channel
	_ = c.Send(slip.List{
		slip.Symbol(":channel-send"),
		slip.Fixnum(ch.ID()),
		slip.List{
			slip.Symbol(":prompt"),
			slip.String(ch.Package().Name),
			slip.String(ch.Package().Name),
			slip.Fixnum(0),
			slip.Fixnum(0),
		},
	})

	return slip.List{
		slip.Fixnum(ch.ID()),
		slip.String(fmt.Sprintf("mrepl-%d", ch.ID())),
	}
}

// handlePing responds to heartbeat requests.
func handlePing(c *Connection, args slip.List) slip.Object {
	// args typically contains (thread tag)
	var tag slip.Object = nil
	if len(args) >= 2 {
		tag = args[1]
	}
	return slip.List{slip.Symbol(":pong"), slip.Fixnum(0), tag}
}
