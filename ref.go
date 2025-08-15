// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// Ref refers to an instance slot value.
type Ref struct {
	Instance Instance
	Key      Symbol
}

func (ref *Ref) String() string {
	return string(ref.Append(nil))
}

// Append the object to a byte slice.
func (ref *Ref) Append(b []byte) []byte {
	value := ref.Get()
	if value == nil {
		return append(b, "nil"...)
	}
	return value.Append(b)
}

// Simplify the Object into simple go types of nil, bool, int64, float64,
// string, []any, map[string]any, or time.Time.
func (ref *Ref) Simplify() (simple any) {
	if value := ref.Get(); value != nil {
		simple = value.Simplify()
	}
	return
}

// Equal returns true if this Object and the other are equal in value.
func (ref *Ref) Equal(other Object) (eq bool) {
	if value := ref.Get(); value != nil {
		eq = value.Equal(other)
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (ref *Ref) Hierarchy() (hierarchy []Symbol) {
	if value := ref.Get(); value != nil {
		hierarchy = value.Hierarchy()
	}
	return
}

// Eval the object.
func (ref *Ref) Eval(s *Scope, depth int) (v Object) {
	if value := ref.Get(); value != nil {
		v = value.Eval(s, depth)
	}
	return
}

// Get then value referenced.
func (ref *Ref) Get() Object {
	value, has := ref.Instance.SlotValue(ref.Key)
	if !has {
		PanicCell(ref.Key,
			"When attempting to read the slot's value, the slot %s is missing from the object %s.",
			ref.Key, ref.Instance)
	}
	return value
}
