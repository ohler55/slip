// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"io"
	"os"
	"strings"
)

var (
	// ErrorOutput backs *error-output*.
	ErrorOutput Object = (*FileStream)(os.Stderr)

	// StandardOutput backs *standard-output*.
	StandardOutput Object = (*FileStream)(os.Stdout)

	// StandardInput backs *standard-input*.
	StandardInput Object = (*FileStream)(os.Stdin)

	// WorkingDir *default-pathname-defaults*
	WorkingDir, _ = os.Getwd()

	// Interactive flag. If true then warnings are output to *standard-output*
	// otherwise they are output to *error-output*.
	Interactive bool

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
		"*print-pretty*":              setPrintPretty,
		"*print-radix*":               setPrintRadix,
		"*print-readably*":            setPrintReadably,
		"*print-right-margin*":        setPrintRightMargin,
		"*standard-output*":           setStandardOutput,
		"*standard-input*":            setStandardInput,
	}
	varValues = map[string]Object{}
	varDocs   = map[string]string{
		"*default-pathname-defaults*": ``,

		"*error-output*": `is a stream used as the default for warnings and errors when not in interaction mode.`,

		"*print-ansi*": ``,

		"*print-array*": ``,

		"*print-base*": ``,

		"*print-case*": ``,

		"*print-circle*": ``,

		"*print-escape*": ``,

		"*print-gensym*": ``,

		"*print-length*": ``,

		"*print-level*": `controls how many levels deep a nested _object_ will print.
If nil no limit is imposed otherwise a positive fixnum specifies the level at which a _#_ is
output in place of the _object_ element.`,

		"*print-lines*": ``,

		"*print-miser-width*": ``,

		"*print-pretty*": ``,

		"*print-radix*": ``,

		"*print-readably*": ``,

		"*print-right-margin*": ``,

		"*standard-output*": `is a stream used as the default output destination for writing.`,

		"*standard-input*": `is a stream used as the default input source for reading.`,
	}
)

func getWorkingDir() Object {
	return String(WorkingDir)
}

func setWorkingDir(value Object) {
	if dir, ok := value.(String); ok {
		WorkingDir = string(dir)
	} else {
		PanicType("*default-pathname-defaults*", value, "string")
	}
}

func getErrorOutput() Object {
	return ErrorOutput
}

func setErrorOutput(value Object) {
	if _, ok := value.(io.Writer); ok {
		ErrorOutput = value
	} else {
		PanicType("*error-output*", value, "stream")
	}
}

func getStandardOutput() Object {
	return StandardOutput
}

func setStandardOutput(value Object) {
	if _, ok := value.(io.Writer); ok {
		StandardOutput = value
	} else {
		PanicType("*standard-output*", value, "stream")
	}
}

func getStandardInput() Object {
	return StandardInput
}

func setStandardInput(value Object) {
	if _, ok := value.(io.Reader); ok {
		StandardInput = value
	} else {
		PanicType("*standard-input*", value, "stream")
	}
}

func GetVar(sym Symbol) (Object, bool) {
	return getVar(strings.ToLower(string(sym)))
}

func getVar(name string) (Object, bool) {
	if value, has := varValues[name]; has {
		return value, true
	}
	if value, has := constantValues[name]; has {
		return value, true
	}
	if f, has := varGets[name]; has {
		return f(), true
	}

	// TBD check functions as well when defined

	return nil, false
}

func SetVar(sym Symbol, value Object) {
	setVar(strings.ToLower(string(sym)), value)
}

func setVar(name string, value Object) {
	if _, has := constantValues[name]; has {
		panic(fmt.Sprintf("%s is a constant and thus can't be set", name))
	}
	if f, has := varSets[name]; has {
		f(value)
	}
	varValues[name] = value
}

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

func RemoveVar(sym Symbol) {
	name := strings.ToLower(string(sym))
	delete(varValues, name)
	delete(varDocs, name)
}

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

func DescribeFunction(sym Symbol) *FuncDoc {
	name := strings.ToLower(string(sym))
	if doc, has := funcDocs[name]; has {
		return doc
	}
	return nil
}
