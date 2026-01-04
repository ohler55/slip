// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// ScopedAppender is an interface for custom appenders that make use of the
// scope and level.
type ScopedAppender interface {
	ScopedAppend(b []byte, s *Scope, p *Printer, level int) []byte
}
