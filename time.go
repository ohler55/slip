// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"strings"
	"time"
)

// TimeSymbol is the symbol with a value of "time".
const TimeSymbol = Symbol("time")

func init() {
	DefConstant(TimeSymbol, TimeSymbol,
		`A _time_ identifies an instant in time backed by a golang time.Time.`)
}

var timeMethods = map[string]func(s *Scope, obj Time, args List, depth int) Object{
	":add":        addTime,
	":components": TimeComponents,
	":describe":   describeTime,
	":elapsed":    elapsedTime,
	":unix": func(s *Scope, obj Time, args List, depth int) Object {
		return DoubleFloat(float64(time.Time(obj).UnixNano()) / float64(time.Second))
	},
}

// Time is a time.Time Object. It might be better in the pkg/gi package but
// then it could not be used in the SimpleObject conversion function so
// instead it is a core (slip) type and the time related functions are in the
// pkg/gi package.
type Time time.Time

// String representation of the Object.
func (obj Time) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Time) Append(b []byte) []byte {
	b = append(b, '@')
	b = time.Time(obj).AppendFormat(b, time.RFC3339Nano)
	return b
}

// Simplify the Object into a time.Time.
func (obj Time) Simplify() interface{} {
	return time.Time(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj Time) Equal(other Object) (eq bool) {
	if to, ok := other.(Time); ok {
		return time.Time(obj).Equal(time.Time(to))
	}
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Time) Hierarchy() []Symbol {
	return []Symbol{TimeSymbol, TrueSymbol}
}

// Eval returns self.
func (obj Time) Eval(s *Scope, depth int) Object {
	return obj
}

// Class of the instance.
func (obj Time) Class() Class {
	return FindClass("time")
}

// Init does nothing.
func (obj Time) Init(scope *Scope, args List, depth int) {
}

// Receive a method invocation from the send function. It is typically
// called by the send function but can be called directly so effectively
// send a method to an instance.
func (obj Time) Receive(s *Scope, message string, args List, depth int) Object {
	f, _ := timeMethods[strings.ToLower(message)]
	if f == nil {
		PanicMethod(Symbol("time"), nil, Symbol(message), "")
	}
	return f(s, obj, args, depth)
}

// HasMethod returns true if the instance handles the named method.
func (obj Time) HasMethod(method string) bool {
	_, has := timeMethods[strings.ToLower(method)]
	return has
}

// Describe the instance in detail.
func (obj Time) Describe(b []byte, indent, right int, ansi bool) []byte {
	// TBD
	return b
}

func describeTime(s *Scope, obj Time, args List, depth int) Object {
	// TBD

	return nil
}

func addTime(s *Scope, obj Time, args List, depth int) Object {
	dur, ok := args[0].(Real)
	if !ok {
		PanicType("duration", args[0], "real")
	}
	return Time(time.Time(obj).Add(time.Duration(dur.RealValue() * float64(time.Second))))
}

func elapsedTime(s *Scope, obj Time, args List, depth int) Object {
	end, ok := args[0].(Time)
	if !ok {
		PanicType("end", args[0], "time")
	}
	return DoubleFloat(float64(time.Time(end).Sub(time.Time(obj))) / float64(time.Second))
}

// TimeComponents returns a list of the time components as (year month day
// hour minute second nanosecond weekday).
func TimeComponents(s *Scope, obj Time, args List, depth int) Object {
	return List{
		Fixnum(time.Time(obj).Year()),
		Fixnum(time.Time(obj).Month()),
		Fixnum(time.Time(obj).Day()),
		Fixnum(time.Time(obj).Hour()),
		Fixnum(time.Time(obj).Minute()),
		Fixnum(time.Time(obj).Second()),
		Fixnum(time.Time(obj).Nanosecond()),
		String(time.Time(obj).Weekday().String()),
	}
}
