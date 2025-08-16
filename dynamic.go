// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Dynamic represents a function defined by a call to defun or lambda.
type Dynamic struct {
	Function
	// Function.Self must point to a Caller.
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
func (f *Dynamic) Simplify() any {
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

// LoadForm returns a form that can be evaluated to create the object.
func (obj *Dynamic) LoadForm() Object {
	form := make(List, len(obj.Args)+1)
	if 0 < len(obj.Name) {
		form[0] = Symbol(obj.Name)
	} else {
		lambda := List{Symbol("lambda")}
		lc := obj.Self.(*Lambda)
		dl := make(List, len(lc.Doc.Args))
		for i, da := range lc.Doc.Args {
			switch {
			case da.Default == nil:
				dl[i] = Symbol(da.Name)
			default:
				dl[i] = List{Symbol(da.Name), da.Default}
			}
		}
		lambda = append(lambda, dl)
		lambda = append(lambda, lc.Forms...)
		form[0] = lambda
	}
	for i, a := range obj.Args {
		if a != nil {
			switch ta := a.(type) {
			case nil:
				// already nil
			case LoadFormer:
				form[i+1] = ta.LoadForm()
			default:
				PanicPrintNotReadable(ta, "Can not make a load form for %s.", ta)
			}
		}
	}
	return form
}
