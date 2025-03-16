// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos

import (
	"math/big"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	builtInClass = Class{
		name:    "built-in-class",
		final:   true,
		noMake:  true,
		methods: flavors.VanillaMethods(),
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
		inherit: []*Class{&numberClass},
	}
	rationalClass = Class{
		name:    "rational",
		final:   true,
		noMake:  true,
		docs:    "built-in rational number class",
		inherit: []*Class{&realClass},
	}
	integerClass = Class{
		name:    "integer",
		final:   true,
		noMake:  true,
		docs:    "built-in integer class",
		inherit: []*Class{&rationalClass},
	}
	fixnumClass = Class{
		name:      "fixnum",
		final:     true,
		noMake:    true,
		docs:      "built-in fixed number class",
		inherit:   []*Class{&integerClass},
		prototype: slip.Fixnum(42),
	}
	octetClass = Class{
		name:      "octet",
		final:     true,
		docs:      "built-in unsigned 8 bit integer class",
		inherit:   []*Class{&integerClass},
		prototype: slip.Octet(42),
	}
	byteClass = Class{
		name:      "byte",
		final:     true,
		docs:      "built-in unsigned 8 bit integer class",
		inherit:   []*Class{&integerClass},
		prototype: slip.Byte(42),
	}
	signedByteClass = Class{
		name:      "signed-byte",
		final:     true,
		docs:      "built-in signed integer class with a configurable number of bits.",
		inherit:   []*Class{&integerClass},
		prototype: &slip.SignedByte{Bytes: []byte{1}, Size: 2, Neg: true},
	}
	unsignedByteClass = Class{
		name:      "unsigned-byte",
		final:     true,
		docs:      "built-in unsigned integer class with a configurable number of bits.",
		inherit:   []*Class{&signedByteClass},
		prototype: &slip.UnsignedByte{Bytes: []byte{1}, Size: 2},
	}
	bitClass = Class{
		name:      "bit",
		final:     true,
		docs:      "built-in one bit unsigned integer class.",
		inherit:   []*Class{&unsignedByteClass},
		prototype: slip.Bit(0),
	}
	bignumClass = Class{
		name:      "bignum",
		final:     true,
		noMake:    true,
		docs:      "built-in fixed number class",
		inherit:   []*Class{&integerClass},
		prototype: (*slip.Bignum)(big.NewInt(42)),
	}
	floatClass = Class{
		name:    "float",
		final:   true,
		noMake:  true,
		docs:    "built-in float number class",
		inherit: []*Class{&realClass},
	}
	doubleFloatClass = Class{
		name:      "double-float",
		final:     true,
		noMake:    true,
		docs:      "built-in double-float number class",
		inherit:   []*Class{&floatClass},
		prototype: slip.DoubleFloat(42.1),
	}
	singleFloatClass = Class{
		name:      "single-float",
		final:     true,
		noMake:    true,
		docs:      "built-in single-float number class",
		inherit:   []*Class{&floatClass},
		prototype: slip.SingleFloat(42.1),
	}
	shortFloatClass = Class{
		name:      "short-float",
		final:     true,
		noMake:    true,
		docs:      "built-in short-float number class",
		inherit:   []*Class{&floatClass},
		prototype: slip.ShortFloat(42.1),
	}
	longFloatClass = Class{
		name:      "long-float",
		final:     true,
		noMake:    true,
		docs:      "built-in long-float number class",
		inherit:   []*Class{&floatClass},
		prototype: (*slip.LongFloat)(big.NewFloat(42.1)),
	}
	ratioClass = Class{
		name:      "ratio",
		final:     true,
		noMake:    true,
		docs:      "built-in ratio class",
		inherit:   []*Class{&rationalClass},
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
		inherit: []*Class{&sequenceClass},
	}
	vectorClass = Class{
		name:    "vector",
		final:   true,
		noMake:  true,
		docs:    "built-in vector class",
		inherit: []*Class{&arrayClass},
	}
	stringClass = Class{
		name:    "string",
		final:   true,
		noMake:  true,
		docs:    "built-in symbol class",
		inherit: []*Class{&vectorClass},
	}
	octetsClass = Class{
		name:      "octets",
		final:     true,
		noMake:    true,
		docs:      "built-in octets class",
		inherit:   []*Class{&vectorClass},
		prototype: slip.Octets{'x'},
	}
	bitVectorClass = Class{
		name:      "bit-vector",
		final:     true,
		noMake:    true,
		docs:      "built-in bit-vector class",
		inherit:   []*Class{&vectorClass},
		prototype: &slip.BitVector{Bytes: []byte{3}, Size: 4},
	}
	complexClass = Class{
		name:      "complex",
		final:     true,
		noMake:    true,
		docs:      "built-in complex number class",
		inherit:   []*Class{&numberClass},
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
		inherit: []*Class{&streamClass},
	}
	inputStreamClass = Class{
		name:    "input-stream",
		final:   true,
		noMake:  true,
		docs:    "built-in input-stream class",
		inherit: []*Class{&streamClass},
	}
	outputStreamClass = Class{
		name:    "output-stream",
		final:   true,
		noMake:  true,
		docs:    "built-in output-stream class",
		inherit: []*Class{&streamClass},
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
		methods: flavors.VanillaMethods(),
	}
	classClass = Class{
		name:    "class",
		final:   true,
		noMake:  true,
		inherit: []*Class{&standardObjectClass},
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
		slots:   map[string]slip.Object{"message": nil},
		final:   true,
		noMake:  true,
		docs:    "built-in condition class",
		inherit: []*Class{&builtInClass},
		InstanceInit: func(inst slip.Instance, obj slip.Object) {
			if self, ok := inst.(*flavors.Instance); ok {
				if cond, ok2 := obj.(slip.Condition); ok2 {
					self.UnsafeLet(slip.Symbol("message"), slip.String(cond.Error()))
				}
			}
		},
	}
	seriousConditionClass = Class{
		name:    "serious-condition",
		final:   true,
		noMake:  true,
		docs:    "built-in serious-condition class",
		inherit: []*Class{&conditionClass},
	}
	errorClass = Class{
		name:    "error",
		final:   true,
		noMake:  true,
		docs:    "built-in error class",
		inherit: []*Class{&seriousConditionClass},
	}
	warningClass = Class{
		name:    "warning",
		final:   true,
		noMake:  true,
		docs:    "built-in warning class",
		inherit: []*Class{&conditionClass},
	}
	arithmeticErrorClass = Class{
		name:    "arithmetic-error",
		final:   true,
		noMake:  true,
		docs:    "built-in arithmetic-error class",
		inherit: []*Class{&errorClass},
	}
	divisionByZeroClass = Class{
		name:    "division-by-zero",
		final:   true,
		noMake:  true,
		docs:    "built-in division-by-zero class",
		inherit: []*Class{&arithmeticErrorClass},
	}
	cellErrorClass = Class{
		name:    "cell-error",
		final:   true,
		noMake:  true,
		docs:    "built-in cell-error class",
		inherit: []*Class{&errorClass},
	}
	classNotFoundClass = Class{
		name:    "class-not-found",
		final:   true,
		noMake:  true,
		docs:    "built-in class-not-found error class",
		inherit: []*Class{&cellErrorClass},
	}
	controlErrorClass = Class{
		name:    "control-error",
		final:   true,
		noMake:  true,
		docs:    "built-in control-error class",
		inherit: []*Class{&errorClass},
	}
	fileErrorClass = Class{
		name:    "file-error",
		final:   true,
		noMake:  true,
		docs:    "built-in file-error class",
		inherit: []*Class{&errorClass},
	}
	methodErrorClass = Class{
		name:    "method-error",
		final:   true,
		noMake:  true,
		docs:    "built-in method-error class",
		inherit: []*Class{&errorClass},
	}
	packageErrorClass = Class{
		name:    "package-error",
		final:   true,
		noMake:  true,
		docs:    "built-in package-error class",
		inherit: []*Class{&errorClass},
	}
	programErrorClass = Class{
		name:    "program-error",
		final:   true,
		noMake:  true,
		docs:    "built-in program-error class",
		inherit: []*Class{&errorClass},
	}
	typeErrorClass = Class{
		name:    "type-error",
		final:   true,
		noMake:  true,
		docs:    "built-in type-error class",
		inherit: []*Class{&errorClass},
	}
	parseErrorClass = Class{
		name:    "parse-error",
		final:   true,
		noMake:  true,
		docs:    "built-in parse-error class",
		inherit: []*Class{&errorClass},
	}
	readerErrorClass = Class{
		name:    "reader-error",
		final:   true,
		noMake:  true,
		docs:    "built-in reader-error class",
		inherit: []*Class{&parseErrorClass},
	}
	unboundSlotClass = Class{
		name:    "unbound-slot",
		final:   true,
		noMake:  true,
		docs:    "built-in unbound-slot error class",
		inherit: []*Class{&cellErrorClass},
	}
	undefinedFunctionClass = Class{
		name:    "undefined-function",
		final:   true,
		noMake:  true,
		docs:    "built-in undefined-function error class",
		inherit: []*Class{&cellErrorClass},
	}
	simpleConditionClass = Class{
		name:    "simple-condition",
		final:   true,
		noMake:  true,
		docs:    "built-in simple-condition class",
		inherit: []*Class{&conditionClass},
	}
	simpleErrorClass = Class{
		name:    "simple-error",
		final:   true,
		noMake:  true,
		docs:    "built-in simple-error class",
		inherit: []*Class{&simpleConditionClass, &errorClass},
	}
	simpleWarningClass = Class{
		name:    "simple-warning",
		final:   true,
		noMake:  true,
		docs:    "built-in simple-warning class",
		inherit: []*Class{&simpleConditionClass, &warningClass},
	}
	simpleTypeErrorClass = Class{
		name:    "simple-type-error",
		final:   true,
		noMake:  true,
		docs:    "built-in simple-type-error class",
		inherit: []*Class{&simpleConditionClass, &typeErrorClass},
	}
)

func init() {
	for _, c := range []*Class{
		&builtInClass,
		&standardObjectClass,
		&symbolClass,
		&numberClass,
		&realClass,
		&rationalClass,
		&integerClass,
		&fixnumClass,
		&octetClass,
		&byteClass,
		&signedByteClass,
		&unsignedByteClass,
		&bitClass,
		&bignumClass,
		&floatClass,
		&doubleFloatClass,
		&singleFloatClass,
		&shortFloatClass,
		&longFloatClass,
		&ratioClass,
		&sequenceClass,
		&arrayClass,
		&vectorClass,
		&stringClass,
		&octetsClass,
		&bitVectorClass,
		&complexClass,
		&characterClass,
		&timeClass,
		&hashTableClass,
		&streamClass,
		&fileStreamClass,
		&inputStreamClass,
		&outputStreamClass,
		&packageClass,
		&classClass,
		&flavorClass,
		&conditionClass,
		&seriousConditionClass,
		&errorClass,
		&warningClass,
		&arithmeticErrorClass,
		&divisionByZeroClass,
		&cellErrorClass,
		&classNotFoundClass,
		&controlErrorClass,
		&fileErrorClass,
		&methodErrorClass,
		&packageErrorClass,
		&programErrorClass,
		&typeErrorClass,
		&parseErrorClass,
		&readerErrorClass,
		&unboundSlotClass,
		&undefinedFunctionClass,
		&simpleConditionClass,
		&simpleErrorClass,
		&simpleTypeErrorClass,
		&simpleWarningClass,
		&bagPathClass,
		&channelClass,
	} {
		slip.RegisterClass(c.name, c)
		c.mergeInherited()
		if c == &conditionClass {
			flavors.DefMethod(c, c.methods, ":message", "", conditionMessageCaller{})
		}
	}
}

type conditionMessageCaller struct{}

func (caller conditionMessageCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	return s.Get("message")
}

func (caller conditionMessageCaller) Docs() string {
	return `__:message__ => _string_

Returns the message of the instance.
`
}
