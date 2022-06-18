// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Dynamic represents the a function defined by a call to defun or lambda.
type Dynamic struct {
	Function
	// Function.Self must point to a LispCaller.
}

// String representation of the Object.
func (f *Dynamic) String() string {
	return string(f.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (f *Dynamic) Append(b []byte) []byte {
	b = append(b, '(')
	if 0 < len(f.Name) {
		b = append(b, f.Name...)
	} else {
		// TBD or have lispcaller (self) append themselves if an object
		//  make lispcaller and object or create a lambda type to wrap it
		lc := f.Self.(*LispCaller)
		list := make(List, len(lc.Forms)+2)
		copy(list, lc.Forms)
		args := make(List, len(lc.Doc.Args))
		for i, da := range lc.Doc.Args {
			args[len(args)-i-1] = Symbol(da.Name)
		}
		list[len(list)-1] = Symbol("lambda")
		list[len(list)-2] = args
		b = Append(b, list)
	}
	for i := len(f.Args) - 1; 0 <= i; i-- {
		b = append(b, ' ')
		b = Append(b, f.Args[i])
	}
	return append(b, ')')
}

// Simplify the function.
func (f *Dynamic) Simplify() interface{} {
	simple := make([]interface{}, 0, len(f.Args)+1)
	if 0 < len(f.Name) {
		simple = append(simple, f.Name)
	} else {
		lc := f.Self.(*LispCaller)
		args := make([]interface{}, len(lc.Doc.Args))
		for i, da := range lc.Doc.Args {
			args[i] = da.Name
		}
		lambda := make([]interface{}, 0, len(lc.Forms)+2)
		lambda = append(lambda, "lambda", args)
		for i := len(lc.Forms) - 1; 0 <= i; i-- {
			form := lc.Forms[i]
			if form == nil {
				lambda = append(lambda, nil)
			} else {
				lambda = append(lambda, form.Simplify())
			}
		}
		simple = append(simple, lambda)
	}
	for i := len(f.Args) - 1; 0 <= i; i-- {
		simple = append(simple, Simplify(f.Args[i]))
	}
	return simple
}
