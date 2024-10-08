// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos

import (
	"math/big"
	"time"

	"github.com/ohler55/slip"
)

var (
	builtInClass = Class{
		name:   "built-in-class",
		final:  true,
		noMake: true,
	}
	symbolClass = Class{
		name:    "symbol",
		final:   true,
		noMake:  true,
		docs:    "built-in symbol class",
		inherit: []*Class{&builtInClass},
	}
	numberClass = Class{
		name:    "number",
		final:   true,
		noMake:  true,
		docs:    "built-in number class",
		inherit: []*Class{&builtInClass},
	}
	realClass = Class{
		name:    "real",
		final:   true,
		noMake:  true,
		docs:    "built-in real number class",
		inherit: []*Class{&builtInClass, &numberClass},
	}
	rationalClass = Class{
		name:    "rational",
		final:   true,
		noMake:  true,
		docs:    "built-in rational number class",
		inherit: []*Class{&builtInClass, &realClass},
	}
	integerClass = Class{
		name:    "integer",
		final:   true,
		noMake:  true,
		docs:    "built-in integer class",
		inherit: []*Class{&builtInClass, &rationalClass},
	}
	fixnumClass = Class{
		name:      "fixnum",
		final:     true,
		noMake:    true,
		docs:      "built-in fixed number class",
		inherit:   []*Class{&builtInClass, &integerClass},
		prototype: slip.Fixnum(42),
	}
	octetClass = Class{
		name:      "octet",
		final:     true,
		docs:      "built-in unsigned 8 bit integer class",
		inherit:   []*Class{&builtInClass, &integerClass},
		prototype: slip.Octet(42),
	}
	bignumClass = Class{
		name:      "bignum",
		final:     true,
		noMake:    true,
		docs:      "built-in fixed number class",
		inherit:   []*Class{&builtInClass, &integerClass},
		prototype: (*slip.Bignum)(big.NewInt(42)),
	}
	floatClass = Class{
		name:    "float",
		final:   true,
		noMake:  true,
		docs:    "built-in float number class",
		inherit: []*Class{&builtInClass, &realClass},
	}
	doubleFloatClass = Class{
		name:      "double-float",
		final:     true,
		noMake:    true,
		docs:      "built-in double-float number class",
		inherit:   []*Class{&builtInClass, &floatClass},
		prototype: slip.DoubleFloat(42.1),
	}
	singleFloatClass = Class{
		name:      "single-float",
		final:     true,
		noMake:    true,
		docs:      "built-in single-float number class",
		inherit:   []*Class{&builtInClass, &floatClass},
		prototype: slip.SingleFloat(42.1),
	}
	shortFloatClass = Class{
		name:      "short-float",
		final:     true,
		noMake:    true,
		docs:      "built-in short-float number class",
		inherit:   []*Class{&builtInClass, &floatClass},
		prototype: slip.ShortFloat(42.1),
	}
	longFloatClass = Class{
		name:      "long-float",
		final:     true,
		noMake:    true,
		docs:      "built-in long-float number class",
		inherit:   []*Class{&builtInClass, &floatClass},
		prototype: (*slip.LongFloat)(big.NewFloat(42.1)),
	}
	ratioClass = Class{
		name:      "ratio",
		final:     true,
		noMake:    true,
		docs:      "built-in ratio class",
		inherit:   []*Class{&builtInClass, &rationalClass},
		prototype: slip.NewRatio(3, 4),
	}
	sequenceClass = Class{
		name:    "sequence",
		final:   true,
		noMake:  true,
		docs:    "built-in sequence class",
		inherit: []*Class{&builtInClass},
	}
	arrayClass = Class{
		name:    "array",
		final:   true,
		noMake:  true,
		docs:    "built-in array class",
		inherit: []*Class{&builtInClass, &sequenceClass},
	}
	vectorClass = Class{
		name:    "vector",
		final:   true,
		noMake:  true,
		docs:    "built-in vector class",
		inherit: []*Class{&builtInClass, &arrayClass},
	}
	stringClass = Class{
		name:    "string",
		final:   true,
		noMake:  true,
		docs:    "built-in symbol class",
		inherit: []*Class{&builtInClass, &vectorClass},
	}
	complexClass = Class{
		name:      "complex",
		final:     true,
		noMake:    true,
		docs:      "built-in complex number class",
		inherit:   []*Class{&builtInClass, &numberClass},
		prototype: slip.Complex(1 + 2i),
	}
	characterClass = Class{
		name:      "character",
		final:     true,
		noMake:    true,
		docs:      "built-in character class",
		inherit:   []*Class{&builtInClass},
		prototype: slip.Character('A'),
	}
	timeClass = Class{
		name:      "time",
		final:     true,
		noMake:    true,
		docs:      "built-in time class",
		inherit:   []*Class{&builtInClass},
		prototype: slip.Time(time.Date(2022, time.April, 1, 0, 0, 0, 0, time.UTC)),
	}
	hashTableClass = Class{
		name:    "hash-table",
		final:   true,
		noMake:  true,
		docs:    "built-in hash-table class",
		inherit: []*Class{&builtInClass},
	}
	streamClass = Class{
		name:    "stream",
		final:   true,
		noMake:  true,
		docs:    "built-in stream class",
		inherit: []*Class{&builtInClass},
	}
	fileStreamClass = Class{
		name:    "file-stream",
		final:   true,
		noMake:  true,
		docs:    "built-in file-stream class",
		inherit: []*Class{&builtInClass, &streamClass},
	}
	inputStreamClass = Class{
		name:    "input-stream",
		final:   true,
		noMake:  true,
		docs:    "built-in input-stream class",
		inherit: []*Class{&builtInClass, &streamClass},
	}
	outputStreamClass = Class{
		name:    "output-stream",
		final:   true,
		noMake:  true,
		docs:    "built-in output-stream class",
		inherit: []*Class{&builtInClass, &streamClass},
	}
	packageClass = Class{
		name:    "package",
		final:   true,
		noMake:  true,
		docs:    "built-in package class",
		inherit: []*Class{&builtInClass},
	}
	standardObjectClass = Class{
		name:    "standard-object",
		final:   true,
		noMake:  true,
		docs:    "built-in super class for all classes",
		inherit: []*Class{&builtInClass},
	}
	classClass = Class{
		name:    "class",
		final:   true,
		noMake:  true,
		inherit: []*Class{&builtInClass, &standardObjectClass},
	}
	channelClass = Class{
		name:    "channel",
		final:   true,
		noMake:  true,
		docs:    "built-in channel class in the Go Integration (gi) package",
		inherit: []*Class{&builtInClass},
	}
	bagPathClass = Class{
		name:    "bag-path",
		final:   true,
		noMake:  true,
		docs:    "built-in bag-path is a JSON path class in the bag package",
		inherit: []*Class{&builtInClass},
	}
	flavorClass = Class{
		name:    "flavor",
		final:   true,
		noMake:  true,
		docs:    "built-in flavor class in the flavors package",
		inherit: []*Class{&builtInClass},
	}
	conditionClass = Class{
		name:    "condition",
		final:   true,
		noMake:  true,
		docs:    "built-in condition class",
		inherit: []*Class{&builtInClass},
	}
	seriousConditionClass = Class{
		name:    "serious-condition",
		final:   true,
		noMake:  true,
		docs:    "built-in serious-condition class",
		inherit: []*Class{&builtInClass, &conditionClass},
	}
	errorClass = Class{
		name:    "error",
		final:   true,
		noMake:  true,
		docs:    "built-in error class",
		inherit: []*Class{&builtInClass, &seriousConditionClass},
	}
	warningClass = Class{
		name:    "warning",
		final:   true,
		noMake:  true,
		docs:    "built-in warning class",
		inherit: []*Class{&builtInClass, &conditionClass},
	}
	arithmeticErrorClass = Class{
		name:    "arithmetic-error",
		final:   true,
		noMake:  true,
		docs:    "built-in arithmetic-error class",
		inherit: []*Class{&builtInClass, &errorClass},
	}
	cellErrorClass = Class{
		name:    "cell-error",
		final:   true,
		noMake:  true,
		docs:    "built-in cell-error class",
		inherit: []*Class{&builtInClass, &errorClass},
	}
	classNotFoundClass = Class{
		name:    "class-not-found",
		final:   true,
		noMake:  true,
		docs:    "built-in class-not-found error class",
		inherit: []*Class{&builtInClass, &cellErrorClass},
	}
	controlErrorClass = Class{
		name:    "control-error",
		final:   true,
		noMake:  true,
		docs:    "built-in control-error class",
		inherit: []*Class{&builtInClass, &errorClass},
	}
	fileErrorClass = Class{
		name:    "file-error",
		final:   true,
		noMake:  true,
		docs:    "built-in file-error class",
		inherit: []*Class{&builtInClass, &errorClass},
	}
	methodErrorClass = Class{
		name:    "method-error",
		final:   true,
		noMake:  true,
		docs:    "built-in method-error class",
		inherit: []*Class{&builtInClass, &errorClass},
	}
	packageErrorClass = Class{
		name:    "package-error",
		final:   true,
		noMake:  true,
		docs:    "built-in package-error class",
		inherit: []*Class{&builtInClass, &errorClass},
	}
	programErrorClass = Class{
		name:    "program-error",
		final:   true,
		noMake:  true,
		docs:    "built-in program-error class",
		inherit: []*Class{&builtInClass, &errorClass},
	}
	typeErrorClass = Class{
		name:    "type-error",
		final:   true,
		noMake:  true,
		docs:    "built-in type-error class",
		inherit: []*Class{&builtInClass, &errorClass},
	}
	parseErrorClass = Class{
		name:    "parse-error",
		final:   true,
		noMake:  true,
		docs:    "built-in parse-error class",
		inherit: []*Class{&builtInClass, &errorClass},
	}
	readerErrorClass = Class{
		name:    "reader-error",
		final:   true,
		noMake:  true,
		docs:    "built-in reader-error class",
		inherit: []*Class{&builtInClass, &parseErrorClass},
	}
	unboundSlotClass = Class{
		name:    "unbound-slot",
		final:   true,
		noMake:  true,
		docs:    "built-in unbound-slot error class",
		inherit: []*Class{&builtInClass, &cellErrorClass},
	}
	undefinedFunctionClass = Class{
		name:    "undefined-function",
		final:   true,
		noMake:  true,
		docs:    "built-in undefined-function error class",
		inherit: []*Class{&builtInClass, &cellErrorClass},
	}
	simpleConditionClass = Class{
		name:    "simple-condition",
		final:   true,
		noMake:  true,
		docs:    "built-in simple-condition class",
		inherit: []*Class{&builtInClass, &conditionClass},
	}
	simpleErrorClass = Class{
		name:    "simple-error",
		final:   true,
		noMake:  true,
		docs:    "built-in simple-error class",
		inherit: []*Class{&builtInClass, &simpleConditionClass, &errorClass},
	}
	simpleWarningClass = Class{
		name:    "simple-warning",
		final:   true,
		noMake:  true,
		docs:    "built-in simple-warning class",
		inherit: []*Class{&builtInClass, &simpleConditionClass, &warningClass},
	}
	simpleTypeErrorClass = Class{
		name:    "simple-type-error",
		final:   true,
		noMake:  true,
		docs:    "built-in simple-type-error class",
		inherit: []*Class{&builtInClass, &simpleConditionClass, &typeErrorClass},
	}
)

func init() {
	for _, c := range []*Class{
		&arithmeticErrorClass,
		&arrayClass,
		&bagPathClass,
		&bignumClass,
		&builtInClass,
		&cellErrorClass,
		&channelClass,
		&characterClass,
		&classClass,
		&classNotFoundClass,
		&complexClass,
		&conditionClass,
		&controlErrorClass,
		&doubleFloatClass,
		&errorClass,
		&fileErrorClass,
		&fileStreamClass,
		&fixnumClass,
		&flavorClass,
		&floatClass,
		&hashTableClass,
		&inputStreamClass,
		&integerClass,
		&longFloatClass,
		&methodErrorClass,
		&numberClass,
		&outputStreamClass,
		&packageClass,
		&packageErrorClass,
		&parseErrorClass,
		&programErrorClass,
		&ratioClass,
		&rationalClass,
		&readerErrorClass,
		&realClass,
		&sequenceClass,
		&seriousConditionClass,
		&shortFloatClass,
		&simpleConditionClass,
		&simpleErrorClass,
		&simpleTypeErrorClass,
		&simpleWarningClass,
		&singleFloatClass,
		&standardObjectClass,
		&streamClass,
		&stringClass,
		&symbolClass,
		&timeClass,
		&typeErrorClass,
		&unboundSlotClass,
		&undefinedFunctionClass,
		&vectorClass,
		&warningClass,
	} {
		allClasses[c.name] = c
	}
}
