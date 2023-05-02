// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import "github.com/ohler55/slip"

type dualCaller interface {
	slip.Caller
	slip.BoundCaller
}

// Daemon is a wrapper around a caller and the documentation for the caller.
type Daemon struct {
	caller dualCaller
	docs   string
}

// Call the caller member.
func (d *Daemon) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return d.caller.Call(s, args, depth)
}

// BoundCall the caller member.
func (d *Daemon) BoundCall(s *slip.Scope, depth int) slip.Object {
	return d.caller.BoundCall(s, depth)
}

// Docs for the daemon.
func (d *Daemon) Docs() string {
	return d.docs
}
