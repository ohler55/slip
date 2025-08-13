// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

const (
	// ListSymbol is the symbol with a value of "list".
	ListSymbol = Symbol("list")

	// ConsSymbol is the symbol with a value of "cons".
	ConsSymbol = Symbol("cons")
)

// List of Objects.
type List []Object

// String representation of the Object.
func (obj List) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj List) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into a []any.
func (obj List) Simplify() any {
	out := make([]any, len(obj))
	for i, o := range obj {
		if o == nil {
			out[i] = nil
		} else {
			out[i] = o.Simplify()
		}
	}
	return out
}

// Equal returns true if this Object and the other are equal in value.
func (obj List) Equal(other Object) (eq bool) {
	if ol, ok := other.(List); ok && len(obj) == len(ol) {
		eq = true
		for i, co := range obj {
			if !ObjectEqual(co, ol[i]) {
				eq = false
				break
			}
		}
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj List) Hierarchy() []Symbol {
	if 0 < len(obj) {
		if _, ok := obj[len(obj)-1].(Tail); ok {
			return []Symbol{ConsSymbol, ListSymbol, SequenceSymbol, TrueSymbol}
		}
	}
	return []Symbol{ListSymbol, SequenceSymbol, TrueSymbol}
}

// SequenceType returns 'list or 'cons if a cons.
func (obj List) SequenceType() Symbol {
	if 0 < len(obj) {
		if _, ok := obj[len(obj)-1].(Tail); ok {
			return ConsSymbol
		}
	}
	return ListSymbol
}

// Length returns the length of the object.
func (obj List) Length() int {
	return len(obj)
}

// Eval panics unless the list is empty.
func (obj List) Eval(s *Scope, depth int) Object {
	if 0 < len(obj) {
		return ListToFunc(s, obj, depth).Eval(s, depth)
	}
	return nil
}

// Car of the list
func (obj List) Car() (car Object) {
	if 0 < len(obj) {
		car = obj[0]
	}
	return
}

// Cdr of the list.
func (obj List) Cdr() (cdr Object) {
	switch len(obj) {
	case 0, 1:
		// leave cdr as nil
	case 2:
		if tail, ok := obj[len(obj)-1].(Tail); ok {
			cdr = tail.Value
		} else {
			cdr = obj[1:]
		}
	default:
		cdr = obj[1:]
	}
	return
}

// LoadForm returns a form that can be evaluated to create the object or
// panics if that is not possible.
func (obj List) LoadForm() Object {
	if 2 <= len(obj) {
		if tail, ok := obj[len(obj)-1].(Tail); ok {
			form := List{Symbol("cons"), nil, nil}
			if obj[len(obj)-2] != nil {
				form[1] = obj[len(obj)-2].LoadForm()
			}
			if tail.Value != nil {
				form[2] = tail.Value.LoadForm()
			}
			if 2 < len(obj) {
				head := make(List, len(obj)-1)
				head[0] = ListSymbol
				for i := 0; i < len(obj)-2; i++ {
					if obj[i] != nil {
						head[i+1] = obj[i].LoadForm()
					}
				}
				form = List{Symbol("append"), head, form}
			}
			return form
		}
	}
	form := make(List, len(obj)+1)
	form[0] = ListSymbol
	for i, v := range obj {
		if v != nil {
			form[i+1] = v.LoadForm()
		}
	}
	return form
}
