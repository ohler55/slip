// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// CheckArgCount panics if the number of arguments is outside the range
// specified.
func CheckArgCount(s *Scope, depth int, obj Object, args List, mn, mx int) {
	if len(args) < mn || (0 <= mx && mx < len(args)) {
		name := ObjectString(obj)
		if f, ok := obj.(Funky); ok {
			name = f.GetName()
			args = f.GetArgs()
		}
		minMaxPanic(s, depth, name, len(args), mn, mx)
	}
}

// CheckSendArgCount panics if the number of arguments is outside the range
// specified.
func CheckSendArgCount(s *Scope, depth int, self Instance, method string, args List, mn, mx int) {
	if len(args) < mn || (0 <= mx && mx < len(args)) {
		minMaxPanic(s, depth, fmt.Sprintf("%s %s", self, method), len(args), mn, mx)
	}
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
