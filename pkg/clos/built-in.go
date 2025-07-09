// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos

import (
	"math/big"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/bag"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	symbolClass = BuiltInClass{
		name: "symbol",
		docs: "built-in symbol class",
	}
	numberClass = BuiltInClass{
		name: "number",
		docs: "built-in number class",
	}
	realClass = BuiltInClass{
		name:    "real",
		docs:    "built-in real number class",
		inherit: &numberClass,
	}
	rationalClass = BuiltInClass{
		name:    "rational",
		docs:    "built-in rational number class",
		inherit: &realClass,
	}
	integerClass = BuiltInClass{
		name:    "integer",
		docs:    "built-in integer class",
		inherit: &rationalClass,
	}
	fixnumClass = BuiltInClass{
		name:      "fixnum",
		docs:      "built-in fixed number class",
		inherit:   &integerClass,
		prototype: slip.Fixnum(42),
	}
	octetClass = BuiltInClass{
		name:      "octet",
		docs:      "built-in unsigned 8 bit integer class",
		inherit:   &integerClass,
		prototype: slip.Octet(42),
	}
	byteClass = BuiltInClass{
		name:      "byte",
		docs:      "built-in unsigned 8 bit integer class",
		inherit:   &integerClass,
		prototype: slip.Byte(42),
	}
	signedByteClass = BuiltInClass{
		name:      "signed-byte",
		docs:      "built-in signed integer class with a configurable number of bits.",
		inherit:   &integerClass,
		prototype: &slip.SignedByte{Bytes: []byte{1}},
	}
	unsignedByteClass = BuiltInClass{
		name:      "unsigned-byte",
		docs:      "built-in unsigned integer class with a configurable number of bits.",
		inherit:   &signedByteClass,
		prototype: &slip.UnsignedByte{Bytes: []byte{1}},
	}
	bitClass = BuiltInClass{
		name:      "bit",
		docs:      "built-in one bit unsigned integer class.",
		inherit:   &unsignedByteClass,
		prototype: slip.Bit(0),
	}
	bignumClass = BuiltInClass{
		name:      "bignum",
		docs:      "built-in fixed number class",
		inherit:   &integerClass,
		prototype: (*slip.Bignum)(big.NewInt(42)),
	}
	floatClass = BuiltInClass{
		name:    "float",
		docs:    "built-in float number class",
		inherit: &realClass,
	}
	doubleFloatClass = BuiltInClass{
		name:      "double-float",
		docs:      "built-in double-float number class",
		inherit:   &floatClass,
		prototype: slip.DoubleFloat(42.1),
	}
	singleFloatClass = BuiltInClass{
		name:      "single-float",
		docs:      "built-in single-float number class",
		inherit:   &floatClass,
		prototype: slip.SingleFloat(42.1),
	}
	shortFloatClass = BuiltInClass{
		name:      "short-float",
		docs:      "built-in short-float number class",
		inherit:   &floatClass,
		prototype: slip.ShortFloat(42.1),
	}
	longFloatClass = BuiltInClass{
		name:      "long-float",
		docs:      "built-in long-float number class",
		inherit:   &floatClass,
		prototype: (*slip.LongFloat)(big.NewFloat(42.1)),
	}
	ratioClass = BuiltInClass{
		name:      "ratio",
		docs:      "built-in ratio class",
		inherit:   &rationalClass,
		prototype: slip.NewRatio(3, 4),
	}
	sequenceClass = BuiltInClass{
		name: "sequence",
		docs: "built-in sequence class",
	}
	arrayClass = BuiltInClass{
		name:    "array",
		docs:    "built-in array class",
		inherit: &sequenceClass,
	}
	vectorClass = BuiltInClass{
		name:    "vector",
		docs:    "built-in vector class",
		inherit: &arrayClass,
	}
	stringClass = BuiltInClass{
		name:    "string",
		docs:    "built-in symbol class",
		inherit: &vectorClass,
	}
	octetsClass = BuiltInClass{
		name:      "octets",
		docs:      "built-in octets class",
		inherit:   &vectorClass,
		prototype: slip.Octets{'x'},
	}
	bitVectorClass = BuiltInClass{
		name:      "bit-vector",
		docs:      "built-in bit-vector class",
		inherit:   &vectorClass,
		prototype: &slip.BitVector{Bytes: []byte{3}, Len: 4},
	}
	complexClass = BuiltInClass{
		name:      "complex",
		docs:      "built-in complex number class",
		inherit:   &numberClass,
		prototype: slip.Complex(1 + 2i),
	}
	characterClass = BuiltInClass{
		name:      "character",
		docs:      "built-in character class",
		prototype: slip.Character('A'),
	}
	timeClass = BuiltInClass{
		name:      "time",
		docs:      "built-in time class",
		prototype: slip.Time(time.Date(2022, time.April, 1, 0, 0, 0, 0, time.UTC)),
	}
	hashTableClass = BuiltInClass{
		name: "hash-table",
		docs: "built-in hash-table class",
	}
	streamClass = BuiltInClass{
		name: "stream",
		docs: "built-in stream class",
	}
	fileStreamClass = BuiltInClass{
		name:    "file-stream",
		docs:    "built-in file-stream class",
		inherit: &streamClass,
	}
	inputStreamClass = BuiltInClass{
		name:    "input-stream",
		docs:    "built-in input-stream class",
		inherit: &streamClass,
	}
	outputStreamClass = BuiltInClass{
		name:    "output-stream",
		docs:    "built-in output-stream class",
		inherit: &streamClass,
	}
	packageClass = BuiltInClass{
		name: "package",
		docs: "built-in package class",
	}
	bagPathClass = BuiltInClass{
		name: "bag-path",
		docs: "built-in bag-path is a JSON path class in the bag package",
		pkg:  &bag.Pkg,
	}
	channelClass = BuiltInClass{
		name: "channel",
		docs: "built-in channel class in the Go Integration (gi) package",
	}
	flavorClass = BuiltInClass{
		name: "flavor",
		docs: "built-in flavor class in the flavors package",
		pkg:  &flavors.Pkg,
	}

	standardObjectClass = Class{ // TBD remove
		name:   "standard-object",
		final:  true,
		noMake: true,
		docs:   "built-in super class for all classes",
	}
	// classClass = Class{
	// 	name:   "class",
	// 	final:  true,
	// 	noMake: true,
	// }
	// conditionClass = Class{
	// 	name:   "condition",
	// 	slots:  map[string]slip.Object{"message": nil},
	// 	final:  true,
	// 	noMake: true,
	// 	docs:   "built-in condition class",
	// 	// inherit: []*Class{&builtInClass}, // TBD
	// 	InstanceInit: func(inst slip.Instance, obj slip.Object) {
	// 		if self, ok := inst.(*flavors.Instance); ok {
	// 			if cond, ok2 := obj.(slip.Condition); ok2 {
	// 				self.UnsafeLet(slip.Symbol("message"), slip.String(cond.Error()))
	// 			}
	// 		}
	// 	},
	// }
	// seriousConditionClass = Class{
	// 	name:    "serious-condition",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in serious-condition class",
	// 	inherit: []*Class{&conditionClass},
	// }
	// errorClass = Class{
	// 	name:    "error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in error class",
	// 	inherit: []*Class{&seriousConditionClass},
	// }
	// warningClass = Class{
	// 	name:    "warning",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in warning class",
	// 	inherit: []*Class{&conditionClass},
	// }
	// arithmeticErrorClass = Class{
	// 	name:    "arithmetic-error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in arithmetic-error class",
	// 	inherit: []*Class{&errorClass},
	// }
	// divisionByZeroClass = Class{
	// 	name:    "division-by-zero",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in division-by-zero class",
	// 	inherit: []*Class{&arithmeticErrorClass},
	// }
	// cellErrorClass = Class{
	// 	name:    "cell-error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in cell-error class",
	// 	inherit: []*Class{&errorClass},
	// }
	// classNotFoundClass = Class{
	// 	name:    "class-not-found",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in class-not-found error class",
	// 	inherit: []*Class{&cellErrorClass},
	// }
	// controlErrorClass = Class{
	// 	name:    "control-error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in control-error class",
	// 	inherit: []*Class{&errorClass},
	// }
	// fileErrorClass = Class{
	// 	name:    "file-error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in file-error class",
	// 	inherit: []*Class{&errorClass},
	// }
	// methodErrorClass = Class{
	// 	name:    "method-error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in method-error class",
	// 	inherit: []*Class{&errorClass},
	// }
	// packageErrorClass = Class{
	// 	name:    "package-error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in package-error class",
	// 	inherit: []*Class{&errorClass},
	// }
	// programErrorClass = Class{
	// 	name:    "program-error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in program-error class",
	// 	inherit: []*Class{&errorClass},
	// }
	// typeErrorClass = Class{
	// 	name:    "type-error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in type-error class",
	// 	inherit: []*Class{&errorClass},
	// }
	// parseErrorClass = Class{
	// 	name:    "parse-error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in parse-error class",
	// 	inherit: []*Class{&errorClass},
	// }
	// readerErrorClass = Class{
	// 	name:    "reader-error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in reader-error class",
	// 	inherit: []*Class{&parseErrorClass},
	// }
	// unboundSlotClass = Class{
	// 	name:    "unbound-slot",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in unbound-slot error class",
	// 	inherit: []*Class{&cellErrorClass},
	// }
	// undefinedFunctionClass = Class{
	// 	name:    "undefined-function",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in undefined-function error class",
	// 	inherit: []*Class{&cellErrorClass},
	// }
	// simpleConditionClass = Class{
	// 	name:    "simple-condition",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in simple-condition class",
	// 	inherit: []*Class{&conditionClass},
	// }
	// simpleErrorClass = Class{
	// 	name:    "simple-error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in simple-error class",
	// 	inherit: []*Class{&simpleConditionClass, &errorClass},
	// }
	// simpleWarningClass = Class{
	// 	name:    "simple-warning",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in simple-warning class",
	// 	inherit: []*Class{&simpleConditionClass, &warningClass},
	// }
	// simpleTypeErrorClass = Class{
	// 	name:    "simple-type-error",
	// 	final:   true,
	// 	noMake:  true,
	// 	docs:    "built-in simple-type-error class",
	// 	inherit: []*Class{&simpleConditionClass, &typeErrorClass},
	// }
)

func init() {
	for _, c := range []*BuiltInClass{
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
		&bagPathClass,
		&flavorClass,
		&channelClass,
	} {
		if c.pkg == nil {
			c.pkg = &slip.CLPkg
		}
		slip.RegisterClass(c.name, c)
		c.buildPrecedence()
	}
	// TBD change to Condition class
	// for _, c := range []*Class{
	// 	&standardObjectClass,
	// 	&classClass, // TBD remove
	// 	&conditionClass,
	// 	&seriousConditionClass,
	// 	&errorClass,
	// 	&warningClass,
	// 	&arithmeticErrorClass,
	// 	&divisionByZeroClass,
	// 	&cellErrorClass,
	// 	&classNotFoundClass,
	// 	&controlErrorClass,
	// 	&fileErrorClass,
	// 	&methodErrorClass,
	// 	&packageErrorClass,
	// 	&programErrorClass,
	// 	&typeErrorClass,
	// 	&parseErrorClass,
	// 	&readerErrorClass,
	// 	&unboundSlotClass,
	// 	&undefinedFunctionClass,
	// 	&simpleConditionClass,
	// 	&simpleErrorClass,
	// 	&simpleTypeErrorClass,
	// 	&simpleWarningClass,
	// } {
	// 	c.pkg = &Pkg
	// 	slip.RegisterClass(c.name, c)
	// 	c.mergeInherited()
	// }
}
