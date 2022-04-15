// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
)

var (
	varGets = map[string]func() Object{
		"*default-pathname-defaults*": getWorkingDir,
		"*error-output*":              getErrorOutput,
		"*print-ansi*":                getPrintANSI,
		"*print-array*":               getPrintArray,
		"*print-base*":                getPrintBase,
		"*print-case*":                getPrintCase,
		"*print-circle*":              getPrintCircle,
		"*print-escape*":              getPrintEscape,
		"*print-gensym*":              getPrintGensym,
		"*print-length*":              getPrintLength,
		"*print-level*":               getPrintLevel,
		"*print-lines*":               getPrintLines,
		"*print-miser-width*":         getPrintMiserWidth,
		"*print-prec*":                getPrintPrec,
		"*print-pretty*":              getPrintPretty,
		"*print-radix*":               getPrintRadix,
		"*print-readably*":            getPrintReadably,
		"*print-right-margin*":        getPrintRightMargin,
		"*standard-input*":            getStandardInput,
		"*standard-output*":           getStandardOutput,
	}
	varSets = map[string]func(Object){
		"*default-pathname-defaults*": setWorkingDir,
		"*error-output*":              setErrorOutput,
		"*print-ansi*":                setPrintANSI,
		"*print-array*":               setPrintArray,
		"*print-base*":                setPrintBase,
		"*print-case*":                setPrintCase,
		"*print-circle*":              setPrintCircle,
		"*print-escape*":              setPrintEscape,
		"*print-gensym*":              setPrintGensym,
		"*print-length*":              setPrintLength,
		"*print-level*":               setPrintLevel,
		"*print-lines*":               setPrintLines,
		"*print-miser-width*":         setPrintMiserWidth,
		"*print-prec*":                setPrintPrec,
		"*print-pretty*":              setPrintPretty,
		"*print-radix*":               setPrintRadix,
		"*print-readably*":            setPrintReadably,
		"*print-right-margin*":        setPrintRightMargin,
		"*standard-output*":           setStandardOutput,
		"*standard-input*":            setStandardInput,
	}
	varValues = map[string]Object{}
	varDocs   = map[string]string{
		"*default-pathname-defaults*": `is the pathname for the current working directory.`,

		"*error-output*": `is a stream used as the default for warnings and errors when not in interaction mode.`,

		"*print-ansi*": `if true ANSI codes are used for interactive displays.`,

		"*print-array*": `controls the format of _arrays_ when printed. If false the content of arrays
is not printed. If true _array_ content is included when displayed.`,

		"*print-base*": `is the base for integer values when printed. The initial value is ten.`,

		"*print-case*": `controls the display of symbols as either upper case, lower case, or capitalized.`,

		"*print-circle*": `is not currently supported.`,

		"*print-escape*": `if _true_ an attempt is made to print _objects_ so they can be read.
For example the 'a' character will be displayed as _#\a_ when true and simply _a_ when false.`,

		"*print-gensym*": `has no effect. The "#:" is never a prefix for _symbols_`,

		"*print-length*": `controls how many elements at a level are printed. When exceeded "..." is
printed instead of the remaining elements.`,

		"*print-level*": `controls how many levels deep a nested _object_ will print.
If nil no limit is imposed otherwise a positive fixnum specifies the level at which a "#" is
output in place of the _object_ element.`,

		"*print-lines*": `controls how many lines of an object will be printed. If the limit is exceeded
a ".." is appended to the last line. Any closing delimiters are still printed.`,

		"*print-miser-width*": `is not currently supported.`,

		"*print-prec*": `controls the precision of float print representation. The precision of the output
is limited to the specified value or the maximum precision of the _float_ type.`,

		"*print-pretty*": `if true will print _object_ in a "pretty" format that is more readable.
If false (_nil_) then minimal whitespace is used for printing.`,

		"*print-radix*": `is a flag indicating base 2, 8, and 16 rational should be prefixed with
#b, #o, or #x respectively. Base 10 integers will include a trailing decimal point. Other values
are base ar printer with the base following the # character such as #3rN when N is the integer
being printed. Base 10 ratios are given a #10r prefix.`,

		"*print-readably*": `if true while print such that output can be read by the parser when possible
and raises an error if not possible to print readably.`,

		"*print-right-margin*": `establishes the right margin for pretty printing.`,

		"*standard-output*": `is a stream used as the default output destination for writing.`,

		"*standard-input*": `is a stream used as the default input source for reading.`,
	}
)

// GetVar get the value bound to the sym argument. It panics if sym is
// unbound.
func GetVar(sym Symbol) (Object, bool) {
	return getVar(strings.ToLower(string(sym)))
}

func getVar(name string) (Object, bool) {

	// TBD check current package Vars first then constants
	// if vv found then check for non-nil val
	//  if non-nil then return
	//  if nil check ref then get and finally return nil

	if value, has := varValues[name]; has {
		return value, true
	}
	if value, has := constantValues[name]; has {
		return value, true
	}
	if f, has := varGets[name]; has {
		return f(), true
	}

	// TBD check functions as well when defined, what gets returned in that case?

	return nil, false
}

// SetVar binds the sym argument to a value.
func SetVar(sym Symbol, value Object) {
	setVar(strings.ToLower(string(sym)), value)
}

func setVar(name string, value Object) {
	if _, has := constantValues[name]; has {
		panic(fmt.Sprintf("%s is a constant and thus can't be set", name))
	}
	if f, has := varSets[name]; has {
		f(value)
		return
	}
	varValues[name] = value
}

// HasVar returns true if the sym argument is bound to a value.
func HasVar(sym Symbol) bool {
	return hasVar(strings.ToLower(string(sym)))
}

func hasVar(name string) bool {
	if _, has := varValues[name]; has {
		return true
	}
	if _, has := constantValues[name]; has {
		return true
	}
	if _, has := varGets[name]; has {
		return true
	}
	return false
}

// RemoveVar removes the binding to the sym argument.
func RemoveVar(sym Symbol) {
	name := strings.ToLower(string(sym))
	delete(varValues, name)
	delete(varDocs, name)
}

// DescribeVar returns the documentation for the variable bound to the sym
// argument.
func DescribeVar(sym Symbol) string {
	name := strings.ToLower(string(sym))
	if doc, has := varDocs[name]; has {
		return doc
	}
	if doc, has := constantDocs[name]; has {
		return doc
	}
	return ""
}

// DescribeFunction returns the documentation for the function bound to the
// sym argument.
func DescribeFunction(sym Symbol) *FuncDoc {
	name := strings.ToLower(string(sym))
	if doc, has := funcDocs[name]; has {
		return doc
	}
	return nil
}
