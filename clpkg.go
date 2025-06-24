// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"io"
	"math"
	"math/big"
	"os"
	"path/filepath"
	"time"
	"unicode"
)

func init() {
	CurrentPackage = &UserPkg
}

// CLPkg is the COMMON-LISP package.
var (
	// big.MaxExp causes a parse failure. The closest that does is 3 less.
	mostPos, _, _  = big.ParseFloat(fmt.Sprintf("1.0e%d", big.MaxExp-3), 10, 10, big.ToNearestAway)
	mostNeg, _, _  = big.ParseFloat(fmt.Sprintf("-1.0e%d", big.MaxExp-3), 10, 10, big.ToNearestAway)
	leastPos, _, _ = big.ParseFloat(fmt.Sprintf("1.0e%d", big.MinExp), 10, 10, big.ToNearestAway)
	leastNeg, _, _ = big.ParseFloat(fmt.Sprintf("-1.0e%d", big.MinExp), 10, 10, big.ToNearestAway)
	epsilon, _, _  = big.ParseFloat(fmt.Sprintf("1.0e%d", big.MinExp), 10, 10, big.ToNearestAway)

	clPkg *Package // set in init
	CLPkg          = Package{
		Name:      "common-lisp",
		Nicknames: []string{"cl"},
		Doc:       "Home of symbols defined by the ANSI LISP language specification.",
		path:      "github.com/ohler55/slip/pkg/cl",
		vars: map[string]*VarVal{
			"*package*": {Get: getCurrentPackage, Set: setCurrentPackage, Doc: "the current package"},
			"*core-pathname*": {
				Val:    nil,
				Const:  true,
				Export: true,
				Doc:    "The absolute pathname of the running SLIP application.",
			},
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
			"*gensym-counter*": {
				Get: getGensymCounter,
				Set: setGensymCounter,
				Doc: "the gensym counter used when calling gensym.",
			},
			"*load-pathname*": {Val: nil, Doc: "load path set during load."},
			"*load-print*":    {Val: nil, Doc: "default value for print during loading."},
			"*load-truename*": {Val: nil, Doc: "load path set during load."},
			"*load-verbose*":  {Val: nil, Doc: "default value for verbose during loading."},
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
			"*trace-output*": {
				Get: getTraceOutput,
				Set: setTraceOutput,
				Doc: "is a stream used as the output destination for writing trace information.",
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
			"*random-state*": {Val: nil},
			"*read-base*": {
				Val: Fixnum(10),
				Doc: "Controls the parsing of integers and ratios.",
			},
			"*read-default-float-format*": {
				Val: DoubleFloatSymbol,
				Doc: "Controls the floating point type when reading a decimal number.",
			},

			"*terminal-io*": {
				Val: nil,
				Doc: "A two-way-stream that accepts input from the keyboard and writes to the display.",
			},

			// Constants
			"call-arguments-limit": {
				Val:   Fixnum(0x100000000000), // Pick a large number for the limit.
				Const: true,
				Doc:   "The maximum number of function call arguments.",
			},
			"lambda-parameters-limit": {
				Val:   Fixnum(0x100000000000), // Pick a large number for the limit.
				Const: true,
				Doc:   "The maximum number of lambda parameters.",
			},
			"multiple-values-limit": {
				Val:   Fixnum(0x1000000000),
				Const: true,
				Doc:   "The maximum number of values that can be returned by a function.",
			},

			string(DoubleFloatSymbol): {
				Val:   DoubleFloatSymbol,
				Const: true,
				Doc: `A _double-float_ represents a decimal _number_ or _float_. It is implemented
as a float64 as defined by IEEE 754 as a double precision decimal with 16 significant
digits and a maximum exponent of 308.`,
			},
			"most-positive-double-float": {
				Val:   DoubleFloat(math.MaxFloat64),
				Const: true,
				Doc:   "The most positive value a _double-float_ can have.",
			},
			"most-negative-double-float": {
				Val:   DoubleFloat(-math.MaxFloat64),
				Const: true,
				Doc:   "The most negative value a _double-float_ can have.",
			},
			"least-positive-double-float": {
				Val:   DoubleFloat(math.SmallestNonzeroFloat64),
				Const: true,
				Doc:   "The smallest non-zero positive value a _double-float_ can have.",
			},
			"least-negative-double-float": {
				Val:   DoubleFloat(-math.SmallestNonzeroFloat64),
				Const: true,
				Doc:   "The smallest non-zero negative value a _double-float_ can have.",
			},
			"least-positive-normalized-double-float": {
				Val:   DoubleFloat(math.SmallestNonzeroFloat64),
				Const: true,
				Doc:   "The smallest non-zero positive value a _double-float_ can have.",
			},
			"least-negative-normalized-double-float": {
				Val:   DoubleFloat(-math.SmallestNonzeroFloat64),
				Const: true,
				Doc:   "The smallest non-zero negative value a _double-float_ can have.",
			},
			"double-float-epsilon": {
				Val:   DoubleFloat(1.1102230246251568e-16),
				Const: true,
				Doc: `The smallest positive _double-float_ such the addition of the epsilon value to
1.0d0 returns a value greater than 1.0d0.`,
			},
			"double-float-negative-epsilon": {
				Val:   DoubleFloat(1.1102230246251568e-16),
				Const: true,
				Doc: `The smallest positive _double-float_ such the subtraction of the epsilon value from
1.0d0 returns a value less than 1.0d0.`,
			},
			"pi": {
				Val:   DoubleFloat(math.Pi),
				Const: true,
				Doc:   "The value of PI.",
			},
			"e": {
				Val:   DoubleFloat(math.E),
				Const: true,
				Doc:   "The value of e.",
			},
			string(SingleFloatSymbol): {
				Val:   SingleFloatSymbol,
				Const: true,
				Doc: `A _single-float_ represents a decimal _number_ or _float_. It is implemented
as a float32 as defined by IEEE 754 as a single precision decimal with 7 significant
digits and a maximum exponent of 38.`,
			},
			"most-positive-single-float": {
				Val:   SingleFloat(math.MaxFloat32),
				Const: true,
				Doc:   "The most positive value a _single-float_ can have.",
			},
			"most-negative-single-float": {
				Val:   SingleFloat(-math.MaxFloat32),
				Const: true,
				Doc:   "The most negative value a _single-float_ can have.",
			},
			"least-positive-single-float": {
				Val:   SingleFloat(math.SmallestNonzeroFloat32),
				Const: true,
				Doc:   "The smallest non-zero positive value a _single-float_ can have.",
			},
			"least-negative-single-float": {
				Val:   SingleFloat(-math.SmallestNonzeroFloat32),
				Const: true,
				Doc:   "The smallest non-zero negative value a _single-float_ can have.",
			},
			"least-positive-normalized-single-float": {
				Val:   SingleFloat(math.SmallestNonzeroFloat32),
				Const: true,
				Doc:   "The smallest non-zero positive value a _single-float_ can have.",
			},
			"least-negative-normalized-single-float": {
				Val:   SingleFloat(-math.SmallestNonzeroFloat32),
				Const: true,
				Doc:   "The smallest non-zero negative value a _single-float_ can have.",
			},
			"single-float-epsilon": {
				Val:   SingleFloat(5.960465e-8),
				Const: true,
				Doc: `The smallest positive _single-float_ such the addition of the epsilon value to
1.0s0 returns a value greater than 1.0s0.`,
			},
			"single-float-negative-epsilon": {
				Val:   SingleFloat(5.960465e-8),
				Const: true,
				Doc: `The smallest positive _single-float_ such the subtraction of the epsilon value from
1.0s0 returns a value less than 1.0s0.`,
			},

			string(ShortFloatSymbol): {
				Val:   ShortFloatSymbol,
				Const: true,
				Doc: `A _short-float_ represents a decimal _number_ or _float_. It is implemented
as a float32 as defined by IEEE 754 as a short precision decimal with 7 significant
digits and a maximum exponent of 38.`,
			},
			"most-positive-short-float": {
				Val:   ShortFloat(math.MaxFloat32),
				Const: true,
				Doc:   "The most positive value a _short-float_ can have.",
			},
			"most-negative-short-float": {
				Val:   ShortFloat(-math.MaxFloat32),
				Const: true,
				Doc:   "The most negative value a _short-float_ can have.",
			},
			"least-positive-short-float": {
				Val:   ShortFloat(math.SmallestNonzeroFloat32),
				Const: true,
				Doc:   "The smallest non-zero positive value a _short-float_ can have.",
			},
			"least-negative-short-float": {
				Val:   ShortFloat(-math.SmallestNonzeroFloat32),
				Const: true,
				Doc:   "The smallest non-zero negative value a _short-float_ can have.",
			},
			"least-positive-normalized-short-float": {
				Val:   ShortFloat(math.SmallestNonzeroFloat32),
				Const: true,
				Doc:   "The smallest non-zero positive value a _short-float_ can have.",
			},
			"least-negative-normalized-short-float": {
				Val:   ShortFloat(-math.SmallestNonzeroFloat32),
				Const: true,
				Doc:   "The smallest non-zero negative value a _short-float_ can have.",
			},
			"short-float-epsilon": {
				Val:   ShortFloat(5.960465e-8),
				Const: true,
				Doc: `The smallest positive _short-float_ such the addition of the epsilon value to
1.0s0 returns a value greater than 1.0s0.`,
			},
			"short-float-negative-epsilon": {
				Val:   ShortFloat(5.960465e-8),
				Const: true,
				Doc: `The smallest positive _short-float_ such the subtraction of the epsilon value from
1.0s0 returns a value less than 1.0s0.`,
			},

			string(LongFloatSymbol): {
				Val:   LongFloatSymbol,
				Const: true,
				Doc: `A _long-float_ represents a decimal _number_ or _float_. It is implemented
as a float64 as defined by IEEE 754 as a long precision decimal with 16 significant
digits and a maximum exponent of 308.`,
			},
			"most-positive-long-float": {
				Val:   (*LongFloat)(mostPos),
				Const: true,
				Doc:   "The most positive value a _long-float_ can have.",
			},
			"most-negative-long-float": {
				Val:   (*LongFloat)(mostNeg),
				Const: true,
				Doc:   "The most negative value a _long-float_ can have.",
			},
			"least-positive-long-float": {
				Val:   (*LongFloat)(leastPos),
				Const: true,
				Doc:   "The smallest non-zero positive value a _long-float_ can have.",
			},
			"least-negative-long-float": {
				Val:   (*LongFloat)(leastNeg),
				Const: true,
				Doc:   "The smallest non-zero negative value a _long-float_ can have.",
			},
			"least-positive-normalized-long-float": {
				Val:   (*LongFloat)(leastPos),
				Const: true,
				Doc:   "The smallest non-zero positive value a _long-float_ can have.",
			},
			"least-negative-normalized-long-float": {
				Val:   (*LongFloat)(leastNeg),
				Const: true,
				Doc:   "The smallest non-zero negative value a _long-float_ can have.",
			},
			"long-float-epsilon": {
				Val:   (*LongFloat)(epsilon),
				Const: true,
				Doc: `The smallest positive _long-float_ such the addition of the epsilon value to
1.0L0 returns a value greater than 1.0L0.`,
			},
			"long-float-negative-epsilon": {
				Val:   (*LongFloat)(epsilon),
				Const: true,
				Doc: `The smallest positive _long-float_ such the subtraction of the epsilon value from
1.0L0 returns a value less than 1.0L0.`,
			},

			string(ArraySymbol): {
				Val:   ArraySymbol,
				Const: true,
				Doc: `An _array_ is an _n_ dimensional collection of _objects_ identified by a _fixnum_
indices on each dimension.`,
			},

			"array-rank-limit": {
				Val:   Fixnum(ArrayMaxRank),
				Const: true,
				Doc:   `The upper bound on the rank of an _array_.`,
			},
			"array-dimension-limit": {
				Val:   Fixnum(ArrayMaxDimension),
				Const: true,
				Doc:   `The upper exclusive bound on each dimension of an _array_.`,
			},
			"array-total-size-limit": {
				Val:   Fixnum(math.MaxInt),
				Const: true,
				Doc:   `The upper bound on the size of an _array_.`,
			},
			string(BignumSymbol): {
				Val:   BignumSymbol,
				Const: true,
				Doc:   `A _bignum_ is a _number_ represented as a bignum of two integers.`,
			},
			string(BitVectorSymbol): {
				Val:   BitVectorSymbol,
				Const: true,
				Doc:   `A _bit-vector_ is a vector of bits.`,
			},
			string(BitSymbol): {
				Val:   BitSymbol,
				Const: true,
				Doc:   `A _bit_ is a one bit integer.`,
			},
			"bit0": {
				Val:   Bit(0),
				Const: true,
				Doc:   `A _bit_ with a value of 0.`,
			},
			"bit1": {
				Val:   Bit(1),
				Const: true,
				Doc:   `A _bit_ with a value of 1.`,
			},
			string(ByteSymbol): {
				Val:   ByteSymbol,
				Const: true,
				Doc:   `A _byte_ represents an 8 bit unsigned integer.`,
			},
			string(CharacterSymbol): {
				Val:   CharacterSymbol,
				Const: true,
				Doc:   `A _character_ is a Unicode character that can be represented by a golang Rune.`,
			},
			"char-code-limit": {
				Val:   Fixnum(unicode.MaxRune + 1),
				Const: true,
				Doc:   `The upper bounds on a _character_ code value.`,
			},
			string(ComplexSymbol): {
				Val:   ComplexSymbol,
				Const: true,
				Doc:   `A _complex_ is a _number_ composed of a real and imaginary part.`,
			},
			string(FileStreamSymbol): {
				Val:   FileStreamSymbol,
				Const: true,
				Doc:   `A _file-stream_ stream backed by a *os.File.`,
			},
			string(FixnumSymbol): {
				Val:   FixnumSymbol,
				Const: true,
				Doc: `A _fixnum_ is an _integer_ in the range from _most-negative-fixnum_ and
_most-positive-fixnum_ inclusive.`,
			},
			"most-positive-fixnum": {
				Val:   Fixnum(math.MaxInt64),
				Const: true,
				Doc:   "The most positive value a _fixnum_ can have.",
			},
			"most-negative-fixnum": {
				Val:   Fixnum(math.MinInt64),
				Const: true,
				Doc:   "The most negative value a _fixnum_ can have.",
			},
			string(FloatSymbol): {
				Val:   FloatSymbol,
				Const: true,
				Doc:   `A _float_ represents a decimal _number_.`,
			},
			string(HashTableSymbol): {
				Val:   HashTableSymbol,
				Const: true,
				Doc: `A _hash-table_ provides a mapping between a key and value. Keys can be a _string_,
_symbol_, or _fixnum_. Only the __eql__ _:test_ is supported.`,
			},
			string(InputStreamSymbol): {
				Val:   InputStreamSymbol,
				Const: true,
				Doc:   `A _input-stream_ stream backed by a io.Reader.`,
			},
			string(IntegerSymbol): {
				Val:   IntegerSymbol,
				Const: true,
				Doc:   `An _integer_ is any whole _number_.`,
			},
			string(IOStreamSymbol): {
				Val:   IOStreamSymbol,
				Const: true,
				Doc:   `A _io-stream_ stream backed by a io.ReadWriter.`,
			},
			string(ListSymbol): {
				Val:   ListSymbol,
				Const: true,
				Doc:   `A _cons_ is a sequence of _objects_.`,
			},
			string(ConsSymbol): {
				Val:   ConsSymbol,
				Const: true,
				Doc:   `A _cons_ is a dotted pair of _objects_ with a _car_ and a _cdr_.`,
			},
			string(NumberSymbol): {
				Val:   NumberSymbol,
				Const: true,
				Doc:   `A _number_ is a combination of the _real_ and _complex_ numbers.`,
			},
			string(OctetSymbol): {
				Val:   OctetSymbol,
				Const: true,
				Doc:   `A _octet_ is an _unsigned 8bit integer_ in the range from 0 and 255 inclusive.`,
			},
			string(OctetsSymbol): {
				Val:   OctetsSymbol,
				Const: true,
				Doc:   `A _octets_ one dimensional array of _octet_.`,
			},
			string(OutputStreamSymbol): {
				Val:   OutputStreamSymbol,
				Const: true,
				Doc:   `A _output-stream_ stream backed by a io.Writer.`,
			},
			string(PackageSymbol): {
				Val:   PackageSymbol,
				Const: true,
				Doc:   `A _package_ represents a namespace.`,
			},
			string(RatioSymbol): {
				Val:   RatioSymbol,
				Const: true,
				Doc:   `A _ratio_ is a _number_ represented as a ratio of two integers.`,
			},
			string(RationalSymbol): {
				Val:   RationalSymbol,
				Const: true,
				Doc:   `A _rational_ is any precise real number (integer or ratio).`,
			},
			string(RealSymbol): {
				Val:   RealSymbol,
				Const: true,
				Doc:   `A _real_ is any _number_ that denotes a quantity.`,
			},
			string(SequenceSymbol): {
				Val:   SequenceSymbol,
				Const: true,
				Doc:   `A _sequence_ is an ordered collection of _objects_.`,
			},
			string(SignedByteSymbol): {
				Val:   SignedByteSymbol,
				Const: true,
				Doc: `A _signed-byte_ is an integer with a specific range defined by the number
of bits in the byte.`,
			},
			string(StreamSymbol): {
				Val:   StreamSymbol,
				Const: true,
				Doc:   `A _stream_ is an object that can be used with input or output function.`,
			},
			string(StringStreamSymbol): {
				Val:   StringStreamSymbol,
				Const: true,
				Doc:   `A _string-stream_ stream backed by character vector.`,
			},
			string(StringSymbol): {
				Val:   StringSymbol,
				Const: true,
				Doc:   `A _string_ is linear collection of _characters_.`,
			},
			string(SymbolSymbol): {
				Val:   SymbolSymbol,
				Const: true,
				Doc:   `A _symbol_ names an _object_.`,
			},
			string(TimeSymbol): {
				Val:   TimeSymbol,
				Const: true,
				Doc:   `A _time_ identifies an instant in time backed by a golang time.Time.`,
			},
			string(TrueSymbol): {
				Val:   TrueSymbol,
				Const: true,
				Doc:   `_t_ represents any non-nil _object_.`,
			},
			string(UnsignedByteSymbol): {
				Val:   UnsignedByteSymbol,
				Const: true,
				Doc: `A _unsigned-byte_ is an integer with a specific range defined by the number
of bits in the byte.`,
			},
			string(VectorSymbol): {
				Val:   VectorSymbol,
				Const: true,
				Doc:   `A _vector_ is a one dimensional array of _objects_.`,
			},
			"boole-1": {
				Val:   Symbol("boole-1"),
				Const: true,
				Doc:   "boole-1",
			},
			"boole-2": {
				Val:   Symbol("boole-2"),
				Const: true,
				Doc:   "boole-2",
			},
			"boole-and": {
				Val:   Symbol("boole-and"),
				Const: true,
				Doc:   "boole-and",
			},
			"boole-andc1": {
				Val:   Symbol("boole-andc1"),
				Const: true,
				Doc:   "boole-andc1",
			},
			"boole-andc2": {
				Val:   Symbol("boole-andc2"),
				Const: true,
				Doc:   "boole-andc2",
			},
			"boole-c1": {
				Val:   Symbol("boole-c1"),
				Const: true,
				Doc:   "boole-c1",
			},
			"boole-c2": {
				Val:   Symbol("boole-c2"),
				Const: true,
				Doc:   "boole-c2",
			},
			"boole-clr": {
				Val:   Symbol("boole-clr"),
				Const: true,
				Doc:   "boole-clr",
			},
			"boole-eqv": {
				Val:   Symbol("boole-eqv"),
				Const: true,
				Doc:   "boole-eqv",
			},
			"boole-ior": {
				Val:   Symbol("boole-ior"),
				Const: true,
				Doc:   "boole-ior",
			},
			"boole-nand": {
				Val:   Symbol("boole-nand"),
				Const: true,
				Doc:   "boole-nand",
			},
			"boole-nor": {
				Val:   Symbol("boole-nor"),
				Const: true,
				Doc:   "boole-nor",
			},
			"boole-orc1": {
				Val:   Symbol("boole-orc1"),
				Const: true,
				Doc:   "boole-orc1",
			},
			"boole-orc2": {
				Val:   Symbol("boole-orc2"),
				Const: true,
				Doc:   "boole-orc2",
			},
			"boole-set": {
				Val:   Symbol("boole-set"),
				Const: true,
				Doc:   "boole-set",
			},
			"boole-xor": {
				Val:   Symbol("boole-xor"),
				Const: true,
				Doc:   "boole-xor",
			},
			"internal-time-units-per-second": {
				Val:   Fixnum(time.Second),
				Const: true,
				Doc:   "Number of nanoseconds in a second. Internal time units are nanoseconds.",
			},
			"*features*": &features,
		},
		lambdas: map[string]*Lambda{},
		funcs:   map[string]*FuncInfo{},
		PreSet:  DefaultPreSet,
		Locked:  true,
	}

	// ErrorOutput backs *error-output*.
	ErrorOutput Object = (*FileStream)(os.Stderr)

	// StandardOutput backs *standard-output*.
	StandardOutput Object = (*FileStream)(os.Stdout)

	// TrqaceOutput backs *trace-output*.
	TraceOutput Object = (*FileStream)(os.Stdout)

	// StandardInput backs *standard-input*.
	StandardInput Object = (*FileStream)(os.Stdin)

	// WorkingDir *default-pathname-defaults*
	WorkingDir, _ = os.Getwd()

	// Interactive flag. If true then warnings are output to *standard-output*
	// otherwise they are output to *error-output*.
	Interactive bool

	// CurrentPackage is the current package.
	CurrentPackage *Package

	gensymCounter = Fixnum(99)
)

func init() {
	clPkg = &CLPkg
	CLPkg.vars["*cl*"] = &VarVal{Val: &CLPkg, Const: true, Doc: "The common-lisp package."}
	CLPkg.vars["*common-lisp*"] = &VarVal{Val: &CLPkg, Const: true, Doc: "The common-lisp package."}
	for name, vv := range CLPkg.vars {
		vv.Pkg = &CLPkg
		vv.Export = true
		vv.name = name
	}
	xpath, _ := os.Executable()
	xpath, _ = filepath.EvalSymlinks(xpath)
	CLPkg.vars["*core-pathname*"].Val = String(xpath)
}

func getCurrentPackage() Object {
	return CurrentPackage
}

func setCurrentPackage(value Object) {
	if pkg, ok := value.(*Package); ok {
		CurrentPackage = pkg
		callSetHooks(clPkg, "*package*")
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
		callSetHooks(clPkg, "*default-pathname-defaults*")
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
		callSetHooks(clPkg, "*error-output*")
	} else {
		PanicType("*error-output*", value, "stream")
	}
}

func getGensymCounter() Object {
	return gensymCounter
}

func setGensymCounter(value Object) {
	if counter, ok := value.(Fixnum); ok {
		gensymCounter = counter
		callSetHooks(clPkg, "*gensym-counter*")
	} else {
		PanicType("*gensym-counter*", value, "fixnum")
	}
}

func getStandardOutput() Object {
	return StandardOutput
}

func setStandardOutput(value Object) {
	if _, ok := value.(io.Writer); ok {
		StandardOutput = value
		callSetHooks(clPkg, "*standard-output*")
	} else {
		PanicType("*standard-output*", value, "stream")
	}
}

func getTraceOutput() Object {
	return TraceOutput
}

func setTraceOutput(value Object) {
	if _, ok := value.(io.Writer); ok {
		TraceOutput = value
		callSetHooks(clPkg, "*trace-output*")
	} else {
		PanicType("*trace-output*", value, "stream")
	}
}

func getStandardInput() Object {
	return StandardInput
}

func setStandardInput(value Object) {
	if _, ok := value.(io.Reader); ok {
		StandardInput = value
		callSetHooks(clPkg, "*standard-input*")
	} else {
		PanicType("*standard-input*", value, "stream")
	}
}
