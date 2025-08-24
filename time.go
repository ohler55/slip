// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"io"
	"sort"
	"strings"
	"time"
)

// TimeSymbol is the symbol with a value of "time".
const TimeSymbol = Symbol("time")

func init() {
	timeMethods[":add"] = addTime
	timeMethods[":components"] = TimeComponents
	timeMethods[":describe"] = describeTime
	timeMethods[":elapsed"] = elapsedTime
	timeMethods[":unix"] = unixTime
}

var timeMethods = map[string]func(s *Scope, obj Time, args List, depth int) Object{}

var timeMethodDocs = map[string]*methDoc{
	":add": {
		args: " (time) => time",
		desc: "Returns the a new time that is _duration_ seconds from _time_. [see __time-add__]",
	},
	":components": {
		args: " () => list",
		desc: `Returns a list of the time components as
(year month day hour minute second nanosecond weekday).`,
	},
	":describe": {
		args: " (&optional output-stream)",
		desc: "Describes the instance and the __time__ class.",
	},
	":elapsed": {
		args: " (end) => real",
		desc: "Returns the elapsed time in seconds from this time to the _end_ time.",
	},
	":unix": {
		args: " (&optional units) => real",
		desc: `Returns the number of seconds or specified units since January 1, 1970 UTC.
Units can be :second, :millisecond, :microsecond, or :nanosecond.`,
	},
}

type methDoc struct {
	args string
	desc string
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
func (obj Time) Simplify() any {
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
	f := timeMethods[strings.ToLower(message)]
	if f == nil {
		UndefinedFunctionPanic(s, depth, Symbol("time"), "%s", message)
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
	b = append(b, indentSpaces[:indent]...)
	if ansi {
		b = append(b, bold...)
	}
	b = obj.Append(b)
	if ansi {
		b = append(b, colorOff...)
	}
	b = append(b, ", an instance of "...)
	if ansi {
		b = append(b, bold...)
	}
	b = append(b, "time"...)
	if ansi {
		b = append(b, colorOff...)
	}
	b = append(b, ",\n"...)
	i2 := indent + 2
	i3 := indent + 4
	i4 := indent + 6
	b = append(b, indentSpaces[:i2]...)
	b = append(b, "Methods:\n"...)
	var keys []string
	for k := range timeMethods {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	for _, k := range keys {
		b = append(b, indentSpaces[:i3]...)
		b = append(b, k...)
		b = append(b, timeMethodDocs[k].args...)
		b = append(b, '\n')
		b = AppendDoc(b, timeMethodDocs[k].desc, i4, right, ansi)
		b = append(b, '\n')

	}
	return b
}

// LoadForm returns a form that can be evaluated to create the object.
func (obj Time) LoadForm() Object {
	return obj
}

func describeTime(s *Scope, obj Time, args List, depth int) Object {
	w, _ := s.Get("*standard-output*").(io.Writer)
	if 0 < len(args) {
		var ok bool
		if w, ok = args[0].(io.Writer); !ok {
			PanicType("describe output-stream", args[0], "output-stream")
		}
	}
	ansi := s.Get("*print-ansi*") != nil
	right := int(s.Get("*print-right-margin*").(Fixnum))

	b := obj.Describe(nil, 0, right, ansi)
	if _, err := w.Write(b); err != nil {
		ss, _ := w.(Stream)
		StreamPanic(s, depth, ss, "write failed: %s", err)
	}
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

func unixTime(s *Scope, obj Time, args List, depth int) (result Object) {
	nano := time.Time(obj).UnixNano()
	if 0 < len(args) {
		switch args[0] {
		case Symbol(":second"), Symbol(":seconds"):
			result = DoubleFloat(float64(nano) / float64(time.Second))
		case Symbol(":millisecond"), Symbol(":milliseconds"):
			result = DoubleFloat(float64(nano) / float64(time.Millisecond))
		case Symbol(":microsecond"), Symbol(":microseconds"):
			result = DoubleFloat(float64(nano) / float64(time.Microsecond))
		case Symbol(":nanosecond"), Symbol(":nanoseconds"):
			result = Fixnum(nano)
		default:
			PanicType("units", args[0], ":second", ":millisecond", ":micorsecond", ":nanosecond")
		}
	} else { // default to seconds
		result = DoubleFloat(float64(nano) / float64(time.Second))
	}
	return
}
