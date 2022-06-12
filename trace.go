// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// Trace turns tracing on or off for the scope and any future sub-scopes.
func Trace(on bool) {
	if on {
		beforeEval = traceBefore
		afterEval = traceAfter
	} else {
		beforeEval = noopBefore
		afterEval = normalAfter
	}
}

func noopBefore(s *Scope, name string, args List, depth int) {
}

func normalAfter(s *Scope, name string, args List, depth int) {
	switch tr := recover().(type) {
	case nil:
	case *Panic:
		tr.Stack = append(tr.Stack, ObjectString(append(args, Symbol(name))))
		panic(tr)
	default:
		panic(&Panic{
			Message: fmt.Sprint(tr), Stack: []string{ObjectString(append(args, Symbol(name)))},
		})
	}
}

func traceBefore(s *Scope, name string, args List, depth int) {
	// TBD format trace
}

func traceAfter(s *Scope, name string, args List, depth int) {
	// TBD format trace
	normalAfter(s, name, args, depth)
}
