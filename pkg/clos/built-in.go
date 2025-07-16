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
)

func defBuiltIns() {
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
}
