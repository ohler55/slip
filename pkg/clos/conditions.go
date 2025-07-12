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

	docSym     = slip.Symbol(":documentation")
	readerSym  = slip.Symbol(":reader")
	initargSym = slip.Symbol(":initarg")
	typeSym    = slip.Symbol(":type")
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
	defCellError()
	defUnboundSlot()
	defUnboundVariable()
	defUndefinedFunction()
	defSimpleCondition()
	defSimpleError()
	defSimpleTypeError()
	defSimpleWarning()
	defClassNotFound()
}

func defCondition() {
	DefConditionClass("condition", slip.List{},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("report"),
				readerSym, slip.Symbol("condition-report"),
				initargSym, slip.Symbol(":report"),
				docSym, slip.String(`Can be a _string_, _lambda_, or a _symbol_ bound to a function. The
function must take two arguments. The first is the condition and the second is a stream. If a _string_
then only the string is written to the stream. The report call is triggered by a call to the
__print-object__ function if __*print-escape*__ is _nil_.`),
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`The __condition__ class is the class all other errors and non-errors inherit from.
Conditions are defined using __define-condition__ and instance of conditions are created with the
__make-condition__ function.`)},
		},
	).Final = true
}

func defWarning() {
	DefConditionClass("warning", slip.List{ConditionSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("message"),
				readerSym, slip.Symbol("warning-message"),
				initargSym, slip.Symbol(":message"),
				docSym, slip.String(`The _message_ slot if for a message that describes the warning.`),
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __warning__ is used to raise warnings.`),
			},
		},
	).Final = true
}

func defSeriousCondition() {
	DefConditionClass("serious-condition", slip.List{ConditionSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`The __serious-condition__ class is the superclass for conditions
considered to be serious but with no additional slots it serves only as an indicator of seriousness.`),
			},
		},
	).Final = true
}

func defError() {
	DefConditionClass("error", slip.List{SeriousConditionSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("message"),
				readerSym, slip.Symbol("error-message"),
				initargSym, slip.Symbol(":message"),
				docSym, slip.String(`The _message_ slot if for a message that describes the error.`),
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
	).Final = true
}

func defArithmeticError() {
	DefConditionClass("arithmetic-error", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("operation"),
				readerSym, slip.Symbol("arithmetic-error-operation"),
				initargSym, slip.Symbol(":operation"),
				docSym, slip.String(`The _operation_ slot indicates the operation that caused the error.`),
			},
			slip.List{
				slip.Symbol("operands"),
				readerSym, slip.Symbol("arithmetic-error-operands"),
				initargSym, slip.Symbol(":operands"),
				docSym, slip.String(`The _operands_ slot is a list of the operands for the
operation triggering the error.`),
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`The __arithmetic-error__ class is for error conditions that occur during
arithmetic operations.`),
			},
		},
	).Final = true
}

func defDivisionByZero() {
	DefConditionClass("division-by-zero", slip.List{ArithmeticErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __division-by-zero__ is an error class that occurs due to a division by zero.`),
			},
		},
	).Final = true
}

func defControlError() {
	DefConditionClass("control-error", slip.List{ErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __control-error__ is a result of an invalid transfer of control such
as a __return-from__ to a tag that is not available.`),
			},
		},
	).Final = true
}

func defFileError() {
	DefConditionClass("file-error", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("pathname"),
				readerSym, slip.Symbol("file-error-pathname"),
				initargSym, slip.Symbol(":pathname"),
				docSym, slip.String(`The _pathname_ slot identifies the file pathname the error occurred for.`),
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __file-error__ occurs during an attempt to open or close a file.`),
			},
		},
	).Final = true
}

func defPackageError() {
	DefConditionClass("package-error", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("package"),
				readerSym, slip.Symbol("package-error-package"),
				initargSym, slip.Symbol(":package"),
				docSym, slip.String(`The _package_ slot identifies the package the error occurred for.`),
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __package-error__ is related to an operation on a package.`),
			},
		},
	).Final = true
}

func defProgramError() {
	DefConditionClass("program-error", slip.List{ErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __program-error__ is used to describe incorrect program syntax.`),
			},
		},
	).Final = true
}

func defTypeError() {
	DefConditionClass("type-error", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("datum"),
				readerSym, slip.Symbol("type-error-datum"),
				initargSym, slip.Symbol(":datum"),
				docSym, slip.String(`The _datum_ slot holds the offending value.`),
			},
			slip.List{
				slip.Symbol("expected-type"),
				readerSym, slip.Symbol("type-error-expected-type"),
				initargSym, slip.Symbol(":expected-type"),
				docSym, slip.String(`The _expected-type_ slot holds the expected type or types.`),
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __type-error__ represents a situation in which value (_datum_) is
not one of the expected types.`),
			},
		},
	).Final = true
}

func defParseError() {
	DefConditionClass("parse-error", slip.List{ErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __parse-error__ is a error related to parsing.`),
			},
		},
	).Final = true
}

func defStreamError() {
	DefConditionClass("stream-error", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("stream"),
				readerSym, slip.Symbol("stream-error-stream"),
				initargSym, slip.Symbol(":stream"),
				docSym, slip.String(`The _stream_ slot references the stream the error occurred on.`),
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __stream-error__ represents error conditions related to a stream.`),
			},
		},
	).Final = true
}

func defReaderError() {
	DefConditionClass("reader-error", slip.List{ParseErrorSymbol, StreamErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __reader-error__ is a error related to reading, tokenizing, and
parsing by the LISP reader.`),
			},
		},
	).Final = true
}

func defCellError() {
	DefConditionClass("cell-error", slip.List{ErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("name"),
				readerSym, slip.Symbol("cell-error-name"),
				initargSym, slip.Symbol(":name"),
				docSym, slip.String(`The _name_ of the cell or slot related to the error.`),
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __cell-error__ represents errors related to a cell or slot.`),
			},
		},
	).Final = true
}

func defUnboundSlot() {
	DefConditionClass("unbound-slot", slip.List{CellErrorSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("instance"),
				readerSym, slip.Symbol("unbound-slot-instance"),
				initargSym, slip.Symbol(":instance"),
				docSym, slip.String(`The _instance_ of the unbound slot.`),
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __unbound-error__ represents errors related to a unbound slot.`),
			},
		},
	).Final = true
}

func defUnboundVariable() {
	DefConditionClass("unbound-variable", slip.List{CellErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __unbound-error__ represents errors related to a unbound variable.`),
			},
		},
	).Final = true
}

func defUndefinedFunction() {
	DefConditionClass("undefined-function", slip.List{CellErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __undefined-error__ represents errors related to a undefined function.`),
			},
		},
	).Final = true
}

func defSimpleCondition() {
	DefConditionClass("simple-condition", slip.List{ConditionSymbol},
		slip.List{ // slot-specifications
			slip.List{
				slip.Symbol("format-control"),
				readerSym, slip.Symbol("simple-condition-format-control"),
				initargSym, slip.Symbol(":format-control"),
				typeSym, slip.Symbol("string"),
				docSym, slip.String(`The _format-control_ slot is __format__ control string used to generate
the condition message.`),
			},
			slip.List{
				slip.Symbol("format-arguments"),
				readerSym, slip.Symbol("simple-condition-format-arguments"),
				initargSym, slip.Symbol(":format-arguments"),
				typeSym, slip.Symbol("list"),
				docSym, slip.String(`The _format-arguments_ are the arguments given to the control string
to form the condition message.`),
			},
		},
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __simple-condition__ represents a condition where the condition message
is formed using __format__ arguments.`),
			},
		},
	).Final = true
}

func defSimpleError() {
	DefConditionClass("simple-error", slip.List{SimpleConditionSymbol, ErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __simple-error__ represents an error where the error message
is formed using __format__ arguments.`),
			},
		},
	).Final = true
}

func defSimpleTypeError() {
	DefConditionClass("simple-type-error", slip.List{SimpleConditionSymbol, TypeErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __simple-type-error__ represents a type error where the error message
is formed using __format__ arguments..`),
			},
		},
	).Final = true
}

func defSimpleWarning() {
	DefConditionClass("simple-warning", slip.List{SimpleConditionSymbol, WarningSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __simple-warning__ represents an warning where the warning message
is formed using __format__ arguments..`),
			},
		},
	).Final = true
}

func defClassNotFound() {
	DefConditionClass("class-not-found", slip.List{CellErrorSymbol},
		slip.List{}, // slot-specifications
		slip.List{ // options
			slip.List{
				docSym,
				slip.String(`A __class-not-found__ represents class not found error.`),
			},
		},
	).Final = true
}
