// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

const (
	// ConditionSymbol is the symbol with a value of "condition".
	ConditionSymbol = slip.Symbol("condition")
	// SeriousConditionSymbol is the symbol with a value of "serious-condition".
	SeriousConditionSymbol = slip.Symbol("serious-condition")
	// WarningSymbol is the symbol with a value of "warning".
	WarningSymbol = slip.Symbol("warning")
	// ErrorSymbol is the symbol with a value of "error".
	ErrorSymbol = slip.Symbol("error")
	// ArithmeticErrorSymbol is the symbol with a value of "arithmetic-error".
	ArithmeticErrorSymbol = slip.Symbol("arithmetic-error")
	// DivisionByZeroSymbol is the symbol with a value of "division-by-zero".
	DivisionByZeroSymbol = slip.Symbol("division-by-zero")
	// ControlErrorSymbol is the symbol with a value of "control-error".
	ControlErrorSymbol = slip.Symbol("control-error")
	// FileErrorSymbol is the symbol with a value of "file-error".
	FileErrorSymbol = slip.Symbol("file-error")
	// PackageErrorSymbol is the symbol with a value of "package-error".
	PackageErrorSymbol = slip.Symbol("package-error")
	// ProgramErrorSymbol is the symbol with a value of "program-error".
	ProgramErrorSymbol = slip.Symbol("program-error")
	// TypeErrorSymbol is the symbol with a value of "type-error".
	TypeErrorSymbol = slip.Symbol("type-error")
	// ParseErrorSymbol is the symbol with a value of "parse-error".
	ParseErrorSymbol = slip.Symbol("parse-error")
	// StreamErrorSymbol is the symbol with a value of "stream-error".
	StreamErrorSymbol = slip.Symbol("stream-error")
	// ReaderErrorSymbol is the symbol with a value of "reader-error".
	ReaderErrorSymbol = slip.Symbol("reader-error")
	// EndOfFileSymbol is the symbol with a value of "end-of-file".
	EndOfFileSymbol = slip.Symbol("end-of-file")
	// CellErrorSymbol is the symbol with a value of "cell-error".
	CellErrorSymbol = slip.Symbol("cell-error")
	// UnboundSlotSymbol is the symbol with a value of "unbound-slot".
	UnboundSlotSymbol = slip.Symbol("unbound-slot")
	// UnboundVariableSymbol is the symbol with a value of "unbound-variable".
	UnboundVariableSymbol = slip.Symbol("unbound-variable")
	// UndefinedFunctionSymbol is the symbol with a value of "undefined-function".
	UndefinedFunctionSymbol = slip.Symbol("undefined-function")
	// SimpleConditionSymbol is the symbol with a value of "simple-condition".
	SimpleConditionSymbol = slip.Symbol("simple-condition")
	// SimpleErrorSymbol is the symbol with a value of "simple-error".
	SimpleErrorSymbol = slip.Symbol("simple-error")
	// SimpleTypeErrorSymbol is the symbol with a value of "simple-type-error".
	SimpleTypeErrorSymbol = slip.Symbol("simple-type-error")
	// SimpleWarningSymbol is the symbol with a value of "simple-warning".
	SimpleWarningSymbol = slip.Symbol("simple-warning")
	// ClassNotFoundSymbol is the symbol with a value of "class-not-found".
	ClassNotFoundSymbol = slip.Symbol("class-not-found")
	// InvalidMethodErrorSymbol is the symbol with a value of "invalid-method-error".
	InvalidMethodErrorSymbol = slip.Symbol("invalid-method-error")
	// PrintNotReadableSymbol is the symbol with a value of "print-not-readable".
	PrintNotReadableSymbol = slip.Symbol("print-not-readable")
	// NoApplicableMethodErrorSymbol is the symbol with a value of "no-applicable-method-error".
	NoApplicableMethodErrorSymbol = slip.Symbol("no-applicable-method-error")

	docSym      = slip.Symbol(":documentation")
	readerSym   = slip.Symbol(":reader")
	initargSym  = slip.Symbol(":initarg")
	typeSym     = slip.Symbol(":type")
	gettableSym = slip.Symbol(":gettable")
	settableSym = slip.Symbol(":settable")
)

func defConditions() {
	defCondition()
	defWarning()
	defSeriousCondition()
	defError()
	defArithmeticError()
	defDivisionByZero()
	defControlError()
	defFileError()
	defPackageError()
	defProgramError()
	defTypeError()
	defParseError()
	defStreamError()
	defReaderError()
	defEndOfFile()
	defCellError()
	defUnboundSlot()
	defUnboundVariable()
	defUndefinedFunction()
	defSimpleCondition()
	defSimpleError()
	defSimpleTypeError()
	defSimpleWarning()
	defClassNotFound()
	defInvalidMethodError()
	defPrintNotReadable()
	defNoApplicableMethodError()
}

func defCondition() {
	s := slip.NewScope()
	DefConditionClass(s, "condition", slip.List{},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("report"),
				readerSym, slip.Symbol("condition-report"),
				initargSym, slip.Symbol(":report"),
				docSym, slip.String(`Can be a _string_, _lambda_, or a _symbol_ bound to a function. The
function must take two arguments. The first is the condition and the second is a stream. If a _string_
then only the string is written to the stream. The report call is triggered by a call to the
__print-object__ function if __*print-escape*__ is _nil_.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`The __condition__ class is the class all other errors and non-errors inherit from.
Conditions are defined using __define-condition__ and instance of conditions are created with the
__make-condition__ function.`)},
		},
		0).Final = true
}

func defWarning() {
	s := slip.NewScope()
	DefConditionClass(s, "warning", slip.List{ConditionSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("message"),
				readerSym, slip.Symbol("warning-message"),
				initargSym, slip.Symbol(":message"),
				docSym, slip.String(`The _message_ slot if for a message that describes the warning.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __warning__ is used to raise warnings.`),
			},
		},
		0).Final = true
}

func defSeriousCondition() {
	s := slip.NewScope()
	DefConditionClass(s, "serious-condition", slip.List{ConditionSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`The __serious-condition__ class is the superclass for conditions
considered to be serious but with no additional slots it serves only as an indicator of seriousness.`),
			},
		},
		0).Final = true
}

func defError() {
	s := slip.NewScope()
	DefConditionClass(s, "error", slip.List{SeriousConditionSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("message"),
				readerSym, slip.Symbol("error-message"),
				initargSym, slip.Symbol(":message"),
				docSym, slip.String(`The _message_ slot if for a message that describes the error.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
			slip.List{
				slip.Symbol("stack"),
				readerSym, slip.Symbol("error-stack"),
				initargSym, slip.Symbol(":stack"),
				docSym, slip.String(`The _stack_ is a representation of the call stack. It is a
list of function call lists describing the call stack to the place the error was created.`),
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`The __error__ class is the superclass for all errors.`),
			},
		},
		0).Final = true
}

func defArithmeticError() {
	s := slip.NewScope()
	DefConditionClass(s, "arithmetic-error", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("operation"),
				readerSym, slip.Symbol("arithmetic-error-operation"),
				initargSym, slip.Symbol(":operation"),
				docSym, slip.String(`The _operation_ slot indicates the operation that caused the error.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
			slip.List{
				slip.Symbol("operands"),
				readerSym, slip.Symbol("arithmetic-error-operands"),
				initargSym, slip.Symbol(":operands"),
				docSym, slip.String(`The _operands_ slot is a list of the operands for the
operation triggering the error.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`The __arithmetic-error__ class is for error conditions that occur during
arithmetic operations.`),
			},
		},
		0).Final = true
}

func defDivisionByZero() {
	s := slip.NewScope()
	DefConditionClass(s, "division-by-zero", slip.List{ArithmeticErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __division-by-zero__ is an error class that occurs due to a division by zero.`),
			},
		},
		0).Final = true
}

func defControlError() {
	s := slip.NewScope()
	DefConditionClass(s, "control-error", slip.List{ErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __control-error__ is a result of an invalid transfer of control such
as a __return-from__ to a tag that is not available.`),
			},
		},
		0).Final = true
}

func defFileError() {
	s := slip.NewScope()
	DefConditionClass(s, "file-error", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("pathname"),
				readerSym, slip.Symbol("file-error-pathname"),
				initargSym, slip.Symbol(":pathname"),
				docSym, slip.String(`The _pathname_ slot identifies the file pathname the error occurred for.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __file-error__ occurs during an attempt to open or close a file.`),
			},
		},
		0).Final = true
}

func defPackageError() {
	s := slip.NewScope()
	DefConditionClass(s, "package-error", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("package"),
				readerSym, slip.Symbol("package-error-package"),
				initargSym, slip.Symbol(":package"),
				docSym, slip.String(`The _package_ slot identifies the package the error occurred for.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __package-error__ is related to an operation on a package.`),
			},
		},
		0).Final = true
}

func defProgramError() {
	s := slip.NewScope()
	DefConditionClass(s, "program-error", slip.List{ErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __program-error__ is used to describe incorrect program syntax.`),
			},
		},
		0).Final = true
}

func defTypeError() {
	s := slip.NewScope()
	DefConditionClass(s, "type-error", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("datum"),
				readerSym, slip.Symbol("type-error-datum"),
				initargSym, slip.Symbol(":datum"),
				docSym, slip.String(`The _datum_ slot holds the offending value.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
			slip.List{
				slip.Symbol("expected-type"),
				readerSym, slip.Symbol("type-error-expected-type"),
				initargSym, slip.Symbol(":expected-type"),
				docSym, slip.String(`The _expected-type_ slot holds the expected type or types.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __type-error__ represents a situation in which value (_datum_) is
not one of the expected types.`),
			},
		},
		0).Final = true
}

func defParseError() {
	s := slip.NewScope()
	DefConditionClass(s, "parse-error", slip.List{ErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __parse-error__ is a error related to parsing.`),
			},
		},
		0).Final = true
}

func defStreamError() {
	s := slip.NewScope()
	DefConditionClass(s, "stream-error", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("stream"),
				readerSym, slip.Symbol("stream-error-stream"),
				initargSym, slip.Symbol(":stream"),
				docSym, slip.String(`The _stream_ slot references the stream the error occurred on.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __stream-error__ represents error conditions related to a stream.`),
			},
		},
		0).Final = true
}

func defReaderError() {
	s := slip.NewScope()
	DefConditionClass(s, "reader-error", slip.List{ParseErrorSymbol, StreamErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __reader-error__ is a error related to reading, tokenizing, and
parsing by the LISP reader.`),
			},
		},
		0).Final = true
}

func defEndOfFile() {
	s := slip.NewScope()
	DefConditionClass(s, "end-of-file", slip.List{StreamErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __end-of-file__ is a error signifying the end of a file has been reached.`),
			},
		},
		0).Final = true
}

func defCellError() {
	s := slip.NewScope()
	DefConditionClass(s, "cell-error", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("name"),
				readerSym, slip.Symbol("cell-error-name"),
				initargSym, slip.Symbol(":name"),
				docSym, slip.String(`The _name_ of the cell or slot related to the error.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __cell-error__ represents errors related to a cell or slot.`),
			},
		},
		0).Final = true
}

func defUnboundSlot() {
	s := slip.NewScope()
	DefConditionClass(s, "unbound-slot", slip.List{CellErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("instance"),
				readerSym, slip.Symbol("unbound-slot-instance"),
				initargSym, slip.Symbol(":instance"),
				docSym, slip.String(`The _instance_ of the unbound slot.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __unbound-error__ represents errors related to a unbound slot.`),
			},
		},
		0).Final = true
}

func defUnboundVariable() {
	s := slip.NewScope()
	DefConditionClass(s, "unbound-variable", slip.List{CellErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __unbound-error__ represents errors related to a unbound variable.`),
			},
		},
		0).Final = true
}

func defUndefinedFunction() {
	s := slip.NewScope()
	DefConditionClass(s, "undefined-function", slip.List{CellErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __undefined-error__ represents errors related to a undefined function.`),
			},
		},
		0).Final = true
}

func defSimpleCondition() {
	s := slip.NewScope()
	DefConditionClass(s, "simple-condition", slip.List{ConditionSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("format-control"),
				readerSym, slip.Symbol("simple-condition-format-control"),
				initargSym, slip.Symbol(":format-control"),
				typeSym, slip.Symbol("string"),
				docSym, slip.String(`The _format-control_ slot is __format__ control string used to generate
the condition message.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
			slip.List{
				slip.Symbol("format-arguments"),
				readerSym, slip.Symbol("simple-condition-format-arguments"),
				initargSym, slip.Symbol(":format-arguments"),
				typeSym, slip.Symbol("list"),
				docSym, slip.String(`The _format-arguments_ are the arguments given to the control string
to form the condition message.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __simple-condition__ represents a condition where the condition message
is formed using __format__ arguments.`),
			},
		},
		0).Final = true
}

func defSimpleError() {
	s := slip.NewScope()
	DefConditionClass(s, "simple-error", slip.List{SimpleConditionSymbol, ErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __simple-error__ represents an error where the error message
is formed using __format__ arguments.`),
			},
		},
		0).Final = true
}

func defSimpleTypeError() {
	s := slip.NewScope()
	DefConditionClass(s, "simple-type-error", slip.List{SimpleConditionSymbol, TypeErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __simple-type-error__ represents a type error where the error message
is formed using __format__ arguments..`),
			},
		},
		0).Final = true
}

func defSimpleWarning() {
	s := slip.NewScope()
	DefConditionClass(s, "simple-warning", slip.List{SimpleConditionSymbol, WarningSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __simple-warning__ represents an warning where the warning message
is formed using __format__ arguments..`),
			},
		},
		0).Final = true
}

func defClassNotFound() {
	s := slip.NewScope()
	DefConditionClass(s, "class-not-found", slip.List{CellErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __class-not-found__ represents class not found error.`),
			},
		},
		0).Final = true
}

func defInvalidMethodError() {
	s := slip.NewScope()
	DefConditionClass(s, "invalid-method-error", slip.List{CellErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("name"),
				slip.Symbol(":reader"), slip.Symbol("method-error-name"),
				slip.Symbol(":reader"), slip.Symbol("method-error-method"),
				slip.Symbol(":initarg"), slip.Symbol(":name"),
				slip.Symbol(":initarg"), slip.Symbol(":method"),
				docSym, slip.String(`The _method_ related to the error.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
			slip.List{
				slip.Symbol("class"),
				slip.Symbol(":reader"), slip.Symbol("method-error-class"),
				slip.Symbol(":initarg"), slip.Symbol(":class"),
				docSym, slip.String(`The _name_ of the class the method is for.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
			slip.List{
				slip.Symbol("qualifier"),
				slip.Symbol(":reader"), slip.Symbol("method-error-qualifier"),
				slip.Symbol(":initarg"), slip.Symbol(":qualifier"),
				docSym, slip.String(`The _qualifier_ of the method related to the error.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __invalid-method-error__ represents errors related to a method.`),
			},
		},
		0).Final = true
}

func defPrintNotReadable() {
	s := slip.NewScope()
	DefConditionClass(s, "print-not-readable", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("object"),
				readerSym, slip.Symbol("print-not-readable-object"),
				initargSym, slip.Symbol(":object"),
				docSym, slip.String(`The _object_ that could not be printed readably.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __print-not-readable__ occurs during an attempt to print a
non-readable object when __*print-readably*__ is non-nil.`),
			},
		},
		0).Final = true
}

func defNoApplicableMethodError() {
	s := slip.NewScope()
	DefConditionClass(s, "no-applicable-method-error", slip.List{CellErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("generic-function"),
				slip.Symbol(":reader"), slip.Symbol("no-applicable-method-error-generic-function"),
				slip.Symbol(":initarg"), slip.Symbol(":generic-function"),
				docSym, slip.String(`The _generic-function_ related to the error.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
			slip.List{
				slip.Symbol("function-arguments"),
				slip.Symbol(":reader"), slip.Symbol("no-applicable-method-error-function-arguments"),
				slip.Symbol(":initarg"), slip.Symbol(":function-arguments"),
				docSym, slip.String(`The _function-arguments_ related to the error.`),
				gettableSym, slip.True,
				settableSym, slip.True,
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __no-applicable-method-error__ represents errors related to a the
lack of a matching method on a generic function.`),
			},
		},
		0).Final = true
}
