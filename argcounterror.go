// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ArgCountCheck panics if the number of arguments is outside the range
// specified.
func ArgCountCheck(obj Object, args List, mn, mx int) {
	if len(args) < mn || (0 <= mx && mx < len(args)) {
		PanicArgCount(obj, mn, mx)
	}
}

// CheckArgCount panics if the number of arguments is outside the range
// specified.
func CheckArgCount(s *Scope, depth int, obj Object, args List, mn, mx int) {
	if len(args) < mn || (0 <= mx && mx < len(args)) {
		ArgCountPanic(s, depth, obj, mn, mx)
	}
}

// SendArgCountCheck panics if the number of arguments is outside the range
// specified.
func SendArgCountCheck(self Instance, method string, args List, mn, mx int) {
	if len(args) < mn || (0 <= mx && mx < len(args)) {
		minMaxPanic(NewScope(), 0, fmt.Sprintf("%s %s", self, method), len(args), mn, mx)
	}
}

// PanicArgCount raises a panic describing the wrong number of arguments to a
// function.
func PanicArgCount(obj Object, mn, mx int, args ...Object) {
	name := ObjectString(obj)
	if f, ok := obj.(Funky); ok {
		name = f.GetName()
		args = f.GetArgs()
	}
	minMaxPanic(NewScope(), 0, name, len(args), mn, mx)
}

// ArgCountPanic raises a panic describing the wrong number of arguments to a
// function.
func ArgCountPanic(s *Scope, depth int, obj Object, mn, mx int, args ...Object) {
	name := ObjectString(obj)
	if f, ok := obj.(Funky); ok {
		name = f.GetName()
		args = f.GetArgs()
	}
	minMaxPanic(s, depth, name, len(args), mn, mx)
}

func minMaxPanic(s *Scope, depth int, name string, cnt, mn, mx int) {
	if mn == mx {
		if cnt < mn {
			ErrorPanic(s, depth, "Too few arguments to %s. %d expected but got %d.", name, mn, cnt)
		} else {
			ErrorPanic(s, depth, "Too many arguments to %s. %d expected but got %d.", name, mn, cnt)
		}
	}
	if cnt < mn {
		ErrorPanic(s, depth, "Too few arguments to %s. At least %d expected but got %d.", name, mn, cnt)
	}
	panic(ErrorNew(s, depth, "Too many arguments to %s. At most %d expected but got %d.", name, mx, cnt))
}
