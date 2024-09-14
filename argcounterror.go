// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ArgCountCheck panics if the number of arguments is outside the range
// specified.
func ArgCountCheck(obj Object, args List, min, max int) {
	if len(args) < min || (0 <= max && max < len(args)) {
		PanicArgCount(obj, min, max)
	}
}

// SendArgCountCheck panics if the number of arguments is outside the range
// specified.
func SendArgCountCheck(self Instance, method string, args List, min, max int) {
	if len(args) < min || (0 <= max && max < len(args)) {
		minMaxPanic(fmt.Sprintf("%s %s", self, method), len(args), min, max)
	}
}

// PanicArgCount raises a panic describing the wrong number of arguments to a
// function.
func PanicArgCount(obj Object, min, max int, args ...Object) {
	name := ObjectString(obj)
	if f, ok := obj.(Funky); ok {
		name = f.GetName()
		args = f.GetArgs()
	}
	minMaxPanic(name, len(args), min, max)
}

func minMaxPanic(name string, cnt, min, max int) {
	if min == max {
		if cnt < min {
			NewPanic("Too few arguments to %s. %d expected but got %d.", name, min, cnt)
		} else {
			NewPanic("Too many arguments to %s. %d expected but got %d.", name, min, cnt)
		}
	}
	if cnt < min {
		NewPanic("Too few arguments to %s. At least %d expected but got %d.", name, min, cnt)
	}
	panic(NewError("Too many arguments to %s. At most %d expected but got %d.", name, max, cnt))
}
