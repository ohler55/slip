// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"io"
	"os"
)

func init() {
	CurrentPackage = &UserPkg
}

// CLPkg is the COMMON-LISP package.
var (
	CLPkg = Package{
		Name:      "common-lisp",
		Nicknames: []string{"cl"},
		Doc:       "Home of symbols defined by the ANSI language spcification.",
		Vars: map[string]*VarVal{
			"*package*": {Get: getCurrentPackage, Set: setCurrentPackage, Doc: "the current package"},
			"*default-pathname-defaults*": {
				Get: getWorkingDir,
				Set: setWorkingDir,
				Doc: "is the pathname for the current working directory.",
			},
			"*package-load-path*": {Val: List{}, Doc: "package load paths"},
			"*error-output*": {
				Get: getErrorOutput,
				Set: setErrorOutput,
				Doc: "is a stream used as the default for warnings and errors when not in interaction mode.",
			},
			"*standard-input*": {
				Get: getStandardInput,
				Set: setStandardInput,
				Doc: "is a stream used as the default input source for reading.",
			},
			"*standard-output*": {
				Get: getStandardOutput,
				Set: setStandardOutput,
				Doc: "is a stream used as the default output destination for writing.",
			},
			"*print-ansi*": {
				Get: getPrintANSI,
				Set: setPrintANSI,
				Doc: "if true ANSI codes are used for interactive displays.",
			},
			"*print-array*": {
				Get: getPrintArray,
				Set: setPrintArray,
				Doc: `controls the format of _arrays_ when printed. If false the content of arrays
is not printed. If true _array_ content is included when displayed.`,
			},
			"*print-base*": {
				Get: getPrintBase,
				Set: setPrintBase,
				Doc: "is the base for integer values when printed. The initial value is ten.",
			},
			"*print-case*": {
				Get: getPrintCase,
				Set: setPrintCase,
				Doc: "controls the display of symbols as either upper case, lower case, or capitalized.",
			},
			"*print-circle*": {
				Get: getPrintCircle,
				Set: setPrintCircle,
				Doc: "is not currently supported.",
			},
			"*print-escape*": {
				Get: getPrintEscape,
				Set: setPrintEscape,
				Doc: `if _true_ an attempt is made to print _objects_ so they can be read.
For example the 'a' character will be displayed as _#\a_ when true and simply _a_ when false.`,
			},
			"*print-gensym*": {
				Get: getPrintGensym,
				Set: setPrintGensym,
				Doc: `has no effect. The "#:" is never a prefix for _symbols_.`,
			},
			"*print-lambda*": {
				Get: getPrintLambda,
				Set: setPrintLambda,
				Doc: `if true lambda expressions are printed as defined otherwise they are printed
similar to other functions.`,
			},
			"*print-length*": {
				Get: getPrintLength,
				Set: setPrintLength,
				Doc: `controls how many elements at a level are printed. When exceeded "..." is
printed instead of the remaining elements.`,
			},
			"*print-level*": {
				Get: getPrintLevel,
				Set: setPrintLevel,
				Doc: `controls how many levels deep a nested _object_ will print.
If nil no limit is imposed otherwise a positive fixnum specifies the level at which a "#" is
output in place of the _object_ element.`,
			},
			"*print-lines*": {
				Get: getPrintLines,
				Set: setPrintLines,
				Doc: `controls how many lines of an object will be printed. If the limit is exceeded
a ".." is appended to the last line. Any closing delimiters are still printed.`,
			},
			"*print-miser-width*": {
				Get: getPrintMiserWidth,
				Set: setPrintMiserWidth,
				Doc: "is not currently supported.",
			},
			"*print-prec*": {
				Get: getPrintPrec,
				Set: setPrintPrec,
				Doc: `controls the precision of float print representation. The precision of the output
is limited to the specified value or the maximum precision of the _float_ type.`,
			},
			"*print-pretty*": {
				Get: getPrintPretty,
				Set: setPrintPretty,
				Doc: `if true will print _object_ in a "pretty" format that is more readable.
If false (_nil_) then minimal whitespace is used for printing.`,
			},
			"*print-radix*": {
				Get: getPrintRadix,
				Set: setPrintRadix,
				Doc: `is a flag indicating base 2, 8, and 16 rational should be prefixed with
#b, #o, or #x respectively. Base 10 integers will include a trailing decimal point. Other values
are base ar printer with the base following the # character such as #3rN when N is the integer
being printed. Base 10 ratios are given a #10r prefix.`,
			},
			"*print-readably*": {
				Get: getPrintReadably,
				Set: setPrintReadably,
				Doc: `if true while print such that output can be read by the parser when possible
and raises an error if not possible to print readably.`,
			},
			"*print-right-margin*": {
				Get: getPrintRightMargin,
				Set: setPrintRightMargin,
				Doc: "establishes the right margin for pretty printing.",
			},
		},
		Lambdas: map[string]*Lambda{},
		Funcs:   map[string]*FuncInfo{},
		PreSet:  DefaultPreSet,
		Locked:  true,
	}

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

	// CurrentPackage is the current package.
	CurrentPackage *Package
)

func init() {
	for _, vv := range CLPkg.Vars {
		vv.Pkg = &CLPkg
	}
	CLPkg.Set("*common-lisp*", &CLPkg)
}

func getCurrentPackage() Object {
	return CurrentPackage
}

func setCurrentPackage(value Object) {
	if pkg, ok := value.(*Package); ok {
		CurrentPackage = pkg
	} else {
		PanicType("*package*", value, "package")
	}
}

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
