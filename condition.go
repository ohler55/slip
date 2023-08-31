// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import (
	"strconv"
	"strings"
	"unsafe"
)

// ConditionSymbol is the symbol with a value of "condition".
const ConditionSymbol = Symbol("condition")

var conditionHierarchy = []Symbol{ConditionSymbol, TrueSymbol}

var conditionMakers = map[string]func(args List) Condition{
	"condition": makeCondition,
}

// Condition is the interface for all conditions including errors and
// warnings. It has no functions that provide useful information other than to
// indicate the type is a Condition which is also an Object.
type Condition interface {
	Object
	error
	// IsCondition need not do anything other than exist.
	IsCondition()
}

// ConditionObj is used to gather a stack trace when panic occurs.
type ConditionObj struct {
	hierarchy []Symbol
}

// IsCondition indicates ConditionObj is a Condition.
func (c *ConditionObj) IsCondition() {
}

// Append the object to a byte slice.
func (c *ConditionObj) Append(b []byte) []byte {
	typeName := "CONDITION"
	if 0 < len(c.hierarchy) {
		typeName = strings.ToUpper(string(c.hierarchy[0]))
	}
	b = append(b, "#<"...)
	b = append(b, typeName...)
	b = append(b, ' ')
	b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(c))), 16)
	return append(b, '>')
}

// Simplify the Object into simple go types of nil, bool, int64, float64,
// string, []any, map[string]any, or time.Time.
func (c *ConditionObj) Simplify() any {
	return string(c.Append(nil))
}

// Equal returns true if this Object and the other are equal in value.
func (c *ConditionObj) Equal(other Object) bool {
	return c == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (c *ConditionObj) Hierarchy() []Symbol {
	if len(c.hierarchy) == 0 {
		c.hierarchy = conditionHierarchy
	}
	return c.hierarchy
}

// SetHierarchy sets the hierarchy. This should only be used by sub-types of
// ConditionObj.
func (c *ConditionObj) SetHierarchy(hierarchy []Symbol) {
	c.hierarchy = hierarchy
}

// Eval the object.
func (c *ConditionObj) Eval(s *Scope, depth int) Object {
	return c
}

// Error returns the panic message.
func (c *ConditionObj) Error() string {
	return string(c.Append(nil))
}

// String returns the panic message.
func (c *ConditionObj) String() string {
	return string(c.Append(nil))
}

func makeCondition(args List) Condition {
	_ = ParseInitList(args)
	return &ConditionObj{hierarchy: conditionHierarchy}
}

// RegisterCondition associates a condition maker function with a condition
// class name.
func RegisterCondition(typeName string, f func(args List) Condition) {
	conditionMakers[strings.ToLower(typeName)] = f
}

// MakeCondition makes a new condition.
func MakeCondition(typeName string, args List) Condition {
	f := conditionMakers[typeName]
	if f == nil {
		f = conditionMakers[strings.ToLower(typeName)]
		if f == nil {
			return nil
		}
	}
	return f(args)
}

// ParseInitList parses an initialization list into a map.
func ParseInitList(args List) map[string]Object {
	kv := map[string]Object{}
	var pos int
	for ; pos < len(args)-1; pos += 2 {
		sym, ok := args[pos].(Symbol)
		if !ok || len(sym) < 1 || sym[0] != ':' {
			PanicType("keyword", args[pos], "keyword")
		}
		keyword := strings.ToLower(string(sym))
		kv[keyword] = args[pos+1]
	}
	if pos < len(args) {
		NewPanic("odd-length initializer list: %s", args)
	}
	return kv
}
