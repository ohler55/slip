// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// ArgCountCheck panics if the number of arguments is outside the range
// specified.
func ArgCountCheck(obj Object, args List, min, max int) {
	if len(args) < min || (0 <= max && max < len(args)) {
		PanicArgCount(obj, min, max)
	}
}

// PanicArgCount raises a panic describing the wrong number of arguments to a
// function.
func PanicArgCount(obj Object, min, max int) {
	f := obj.(Funky)
	args := f.GetArgs()
	if min == max {
		if len(args) < min {
			NewPanic("Too few arguments to %s. %d expected but got %d.", f.GetName(), min, len(args))
		} else {
			NewPanic("Too many arguments to %s. %d expected but got %d.", f.GetName(), min, len(args))
		}
	}
	if len(args) < min {
		NewPanic("Too few arguments to %s. At least %d expected but got %d.", f.GetName(), min, len(args))
	}
	panic(NewError("Too many arguments to %s. At most %d expected but got %d.", f.GetName(), max, len(args)))
}
