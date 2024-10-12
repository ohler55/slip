// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slip

type MethodInvoker interface {
	// InvokeMethod on an object. Typically the invoker is a clos Class.
	InvokeMethod(obj Object, s *Scope, message string, args List, depth int) Object
}
