// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Dynamic represents a function defined by a call to defun or lambda.
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
		b = f.Self.(Object).Append(b)
	}
	for _, a := range f.Args {
		b = append(b, ' ')
		b = Append(b, a)
	}
	return append(b, ')')
}

// Simplify the function.
func (f *Dynamic) Simplify() interface{} {
	simple := make([]any, 0, len(f.Args)+1)
	if 0 < len(f.Name) {
		simple = append(simple, f.Name)
	} else {
		lc := f.Self.(*Lambda)
		args := make([]any, len(lc.Doc.Args))
		for i, da := range lc.Doc.Args {
			args[i] = da.Name
		}
		lambda := make([]any, 0, len(lc.Forms)+2)
		lambda = append(lambda, "lambda", args)
		for _, form := range lc.Forms {
			if form == nil {
				lambda = append(lambda, nil)
			} else {
				lambda = append(lambda, form.Simplify())
			}
		}
		simple = append(simple, lambda)
	}
	for _, a := range f.Args {
		simple = append(simple, Simplify(a))
	}
	return simple
}
