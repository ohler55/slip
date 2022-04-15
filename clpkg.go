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
		Name:      "COMMON-LISP",
		Nicknames: []string{"CL"},
		Doc:       "Home of symbols defined by the ANSI language spcification.",
		Vars: map[string]*VarVal{
			"*package*": {Get: getCurrentPackage, Set: setCurrentPackage, Doc: "the current package"},
			"*default-pathname-defaults*": {
				Get: getWorkingDir,
				Set: setWorkingDir,
				Doc: "is the pathname for the current working directory.",
			},
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
		},
		funcCreators: map[string]func(args List) Object{},
		funcDocs:     map[string]*FuncDoc{},
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
