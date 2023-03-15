// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

const (
	// ListSymbol is the symbol with a value of "list".
	ListSymbol = Symbol("list")

	// ConsSymbol is the symbol with a value of "cons".
	ConsSymbol = Symbol("cons")
)

func init() {
	DefConstant(ListSymbol, ListSymbol, `A _cons_ is a sequence of _objects_.`)
	DefConstant(ConsSymbol, ConsSymbol,
		`A _cons_ is a dotted pair of _objects_ with a _car_ and a _cdr_.`)
}

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

// Simplify the Object into a []interface{}.
func (obj List) Simplify() interface{} {
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
