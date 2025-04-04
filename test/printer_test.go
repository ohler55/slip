// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"io"
	"math/big"
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefaultPrinter(t *testing.T) {
	tt.NotNil(t, slip.DefaultPrinter())
}

func TestPrintANSI(t *testing.T) {
	key := slip.Symbol("*print-ansi*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.NotNil(t, val)

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	(&sliptest.Function{
		Source: `*print-ansi*`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source:    `nothing`,
		PanicType: slip.Symbol("unbound-variable"),
	}).Test(t)
}

func TestPrintArray(t *testing.T) {
	key := slip.Symbol("*print-array*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.True, val)

	a0 := slip.NewArray([]int{}, slip.TrueSymbol, slip.Fixnum(0), nil, true)
	a1 := slip.NewVector(3, slip.TrueSymbol, slip.Fixnum(1), nil, true)
	a := testArray()
	out := slip.Append([]byte{}, a0)
	tt.Equal(t, "#0Anil", string(out))
	out = slip.Append([]byte{}, a1)
	tt.Equal(t, "#(1 1 1)", string(out))
	out = slip.Append([]byte{}, a)
	tt.Equal(t, "#3A(((0 1 2 3) (4 5 6 7) (8 9 10 11)) ((12 13 14 15) (16 17 18 19) (20 21 22 23)))", string(out))

	slip.SetVar(key, nil)
	val, _ = slip.GetVar(key)
	tt.Nil(t, val)

	out = slip.Append([]byte{}, a0)
	tt.Equal(t, "#<(ARRAY T NIL)>", string(out))
	out = slip.Append([]byte{}, a1)
	tt.Equal(t, "#<(VECTOR 3)>", string(out))
	out = slip.Append([]byte{}, a)
	tt.Equal(t, "#<(ARRAY T (2 3 4))>", string(out))

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)
}

func TestPrintBase(t *testing.T) {
	key := slip.Symbol("*print-base*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.Fixnum(16))
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.Fixnum(16), val)

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	tt.Panic(t, func() { slip.SetVar(key, slip.True) })

	radixKey := slip.Symbol("*print-radix*")
	origRadix, _ := slip.GetVar(radixKey)
	defer slip.SetVar(radixKey, origRadix)
	slip.SetVar(radixKey, slip.True)

	obj := slip.Fixnum(37)
	rat := slip.NewRatio(13, 37)
	big := slip.NewBignum(37)
	for _, pair := range []*struct {
		base int
		fixX string
		ratX string
		bigX string
	}{
		{base: 2, fixX: "#b100101", ratX: "#b1101/100101", bigX: "#b100101"},
		{base: 3, fixX: "#3r1101", ratX: "#3r111/1101", bigX: "#3r1101"},
		{base: 8, fixX: "#o45", ratX: "#o15/45", bigX: "#o45"},
		{base: 10, fixX: "37.", ratX: "#10r13/37", bigX: "37."},
		{base: 16, fixX: "#x25", ratX: "#xd/25", bigX: "#x25"},
	} {
		slip.SetVar(key, slip.Fixnum(pair.base))
		tt.Equal(t, pair.fixX, string(slip.Append([]byte{}, obj)), "%d: printer append", pair.base)
		tt.Equal(t, pair.fixX, obj.String(), "base %d obj.Append()", pair.base)

		tt.Equal(t, pair.ratX, string(slip.Append([]byte{}, rat)), pair.base, ": printer append")
		tt.Equal(t, pair.ratX, rat.String(), "base %d obj.Append()", pair.base)

		tt.Equal(t, pair.bigX, string(slip.Append([]byte{}, big)), "%d: printer append", pair.base)
		tt.Equal(t, pair.bigX, big.String(), "base %d obj.Append()", pair.base)
	}
}

func TestPrintCase(t *testing.T) {
	key := slip.Symbol("*print-case*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.Symbol(":downcase"))
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.Symbol(":downcase"), val)

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	tt.Panic(t, func() { slip.SetVar(key, slip.True) })

	obj := slip.Symbol("cYmBal")
	for _, pair := range []*struct {
		sym    slip.Object
		expect string
	}{
		{sym: slip.Symbol(":downcase"), expect: "cymbal"},
		{sym: slip.Symbol(":upcase"), expect: "CYMBAL"},
		{sym: slip.Symbol(":capitalize"), expect: "Cymbal"},
		{sym: nil, expect: "cYmBal"},
	} {
		slip.SetVar(key, pair.sym)
		tt.Equal(t, pair.expect, string(slip.Append([]byte{}, obj)), "%s: printer append", pair.sym)
		tt.Equal(t, pair.expect, obj.String(), "case %s obj.Append()", pair.sym)
	}
}

func TestPrintCircle(t *testing.T) {
	key := slip.Symbol("*print-circle*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.True, val)

	slip.SetVar(key, nil)
	val, _ = slip.GetVar(key)
	tt.Nil(t, val)

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)
}

func TestPrintEscape(t *testing.T) {
	key := slip.Symbol("*print-escape*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, nil)
	val, _ := slip.GetVar(key)
	tt.Nil(t, val)

	obj := slip.Character('X')
	tt.Equal(t, "X", string(slip.Append([]byte{}, obj)))

	slip.SetVar(key, slip.True)
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.True, val)

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	obj = slip.Character('X')
	tt.Equal(t, "#\\X", string(slip.Append([]byte{}, obj)))
}

func TestPrintGensym(t *testing.T) {
	key := slip.Symbol("*print-gensym*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.True, val)

	slip.SetVar(key, nil)
	val, _ = slip.GetVar(key)
	tt.Nil(t, val)

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)
}

func TestPrintLambda(t *testing.T) {
	key := slip.Symbol("*print-lambda*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.True, val)

	scope := slip.NewScope()
	lam := slip.ReadString("(lambda (x y) (+ x y))", scope).Eval(scope, nil)
	out := slip.Append([]byte{}, lam)
	tt.Equal(t, "(lambda (x y) (+ x y))", string(out))

	slip.SetVar(key, nil)
	val, _ = slip.GetVar(key)
	tt.Nil(t, val)

	out = slip.Append([]byte{}, lam)
	tt.Equal(t, `/^#<function \(lambda \(x y\)\) \{[0-9a-f]+\}>/`, string(out))

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)
}

func TestPrintLength(t *testing.T) {
	key := slip.Symbol("*print-length*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, nil)
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Nil(t, val)

	var list slip.List
	for i := 0; i <= 10; i++ {
		list = append(list, slip.Fixnum(i))
	}
	out := slip.Append([]byte{}, list)
	tt.Equal(t, "(0 1 2 3 4 5 6 7 8 9 10)", string(out))

	slip.SetVar(key, slip.Fixnum(10))
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.Fixnum(10), val)

	out = slip.Append([]byte{}, list)
	tt.Equal(t, "(0 1 2 3 4 5 6 7 8 9 ...)", string(out))

	prettyKey := slip.Symbol("*print-pretty*")
	prettyOrig, _ := slip.GetVar(prettyKey)
	defer slip.SetVar(prettyKey, prettyOrig)
	slip.SetVar(prettyKey, nil)

	out = slip.Append([]byte{}, list)
	tt.Equal(t, "(0 1 2 3 4 5 6 7 8 9 ...)", string(out))

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	tt.Panic(t, func() { slip.SetVar(key, slip.Fixnum(-10)) })
}

func TestPrintPrettyLength(t *testing.T) {
	key := slip.Symbol("*print-length*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)
	slip.SetVar(key, slip.Fixnum(5))

	pkey := slip.Symbol("*print-pretty*")
	porig, _ := slip.GetVar(pkey)
	defer slip.SetVar(pkey, porig)
	slip.SetVar(pkey, slip.True)

	var list slip.List
	for i := 0; i <= 10; i++ {
		list = append(list, slip.Fixnum(i))
	}
	out := slip.Append([]byte{}, list)
	tt.Equal(t, "(0 1 2 3 4 ...)", string(out))
}

func TestPrintLevel(t *testing.T) {
	key := slip.Symbol("*print-level*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, nil)
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Nil(t, val)

	var list slip.List
	for i := 7; 0 <= i; i-- {
		list = slip.List{slip.Fixnum(i), list}
	}
	out := slip.Append([]byte{}, list)
	tt.Equal(t, "(0 (1 (2 (3 (4 (5 (6 (7 ()))))))))", string(out))

	slip.SetVar(key, slip.Fixnum(6))
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.Fixnum(6), val)

	out = slip.Append([]byte{}, list)
	tt.Equal(t, "(0 (1 (2 (3 (4 (5 #))))))", string(out))

	prettyKey := slip.Symbol("*print-pretty*")
	prettyOrig, _ := slip.GetVar(prettyKey)
	defer slip.SetVar(prettyKey, prettyOrig)
	slip.SetVar(prettyKey, nil)

	out = slip.Append([]byte{}, list)
	tt.Equal(t, "(0 (1 (2 (3 (4 (5 #))))))", string(out))

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	tt.Panic(t, func() { slip.SetVar(key, slip.Fixnum(-10)) })
}

func TestPrintLines(t *testing.T) {
	key := slip.Symbol("*print-lines*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, nil)
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Nil(t, val)

	widthKey := slip.Symbol("*print-right-margin*")
	origWidth, _ := slip.GetVar(widthKey)
	defer slip.SetVar(widthKey, origWidth)
	slip.SetVar(widthKey, slip.Fixnum(10))

	var list slip.List
	for i := 7; 0 <= i; i-- {
		list = slip.List{slip.Fixnum(i), list}
	}
	out := slip.Append([]byte{}, list)
	tt.Equal(t, `(0
 (1
  (2
   (3
    (4
     (5
      (6
       (7
        ()))))))))`, string(out))

	slip.SetVar(key, slip.Fixnum(6))
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.Fixnum(6), val)

	out = slip.Append([]byte{}, list)
	tt.Equal(t, `(0
 (1
  (2
   (3
    (4
     (5 ..))))))`, string(out))

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	tt.Panic(t, func() { slip.SetVar(key, slip.Fixnum(-10)) })
}

func TestPrintMiserWidth(t *testing.T) {
	key := slip.Symbol("*print-miser-width*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.Fixnum(10))
	val, _ := slip.GetVar(key)
	tt.Equal(t, slip.Fixnum(10), val)

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	tt.Panic(t, func() { slip.SetVar(key, slip.Fixnum(-10)) })
}

func TestPrintPrec(t *testing.T) {
	key := slip.Symbol("*print-prec*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	readablyKey := slip.Symbol("*print-readably*")
	readablyOrig, _ := slip.GetVar(readablyKey)
	defer slip.SetVar(readablyKey, readablyOrig)

	slip.SetVar(readablyKey, nil)
	slip.SetVar(key, slip.Fixnum(6))
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.Fixnum(6), val)

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	tt.Panic(t, func() { slip.SetVar(key, slip.True) })

	sf := slip.SingleFloat(1.234567890123456789e20)
	df := slip.DoubleFloat(1.234567890123456789e20)
	// precision is number of bit, not base 10
	bf, _, _ := big.ParseFloat("1.234567890123456789e20", 10, 100, big.ToNearestAway)
	lf := (*slip.LongFloat)(bf)
	tt.Equal(t, "1.23457e+20", string(slip.Append([]byte{}, sf)), "%s: printer append", sf)
	tt.Equal(t, "1.23457e+20", string(slip.Append([]byte{}, df)), "%s: printer append", df)
	tt.Equal(t, "1.23457e+20", string(slip.Append([]byte{}, lf)), "%s: printer append", df)

	slip.SetVar(readablyKey, slip.True)
	tt.Equal(t, "1.234568s+20", string(slip.Append([]byte{}, sf)), "%s: printer append", df)
	tt.Equal(t, "1.234568d+20", string(slip.Append([]byte{}, df)), "%s: printer append", df)
	tt.Equal(t, "1.234568L+20", string(slip.Append([]byte{}, lf)), "%s: printer append", df)

	slip.SetVar(key, slip.Fixnum(20))
	tt.Equal(t, "1.2345679s+20",
		string(slip.Append([]byte{}, sf)), "%s: printer append readably with prec 20", sf)
	tt.Equal(t, "1.2345678901234568d+20",
		string(slip.Append([]byte{}, df)), "%s: printer append readably with prec 20", df)
	tt.Equal(t, "1.23456789012345678900L+20",
		string(slip.Append([]byte{}, lf)), "%s: printer append readably with prec 20", df)

	slip.SetVar(readablyKey, nil)
	tt.Equal(t, "1.2345679s+20", string(slip.Append([]byte{}, sf)), "%s: printer append with prec 20", sf)
	tt.Equal(t, "1.2345678901234568d+20", string(slip.Append([]byte{}, df)), "%s: printer append with prec 20", df)

}

func TestPrintPretty(t *testing.T) {
	key := slip.Symbol("*print-pretty*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.True, val)

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	widthKey := slip.Symbol("*print-right-margin*")
	origWidth, _ := slip.GetVar(widthKey)
	defer slip.SetVar(widthKey, origWidth)
	slip.SetVar(widthKey, slip.Fixnum(20))

	obj := makeTestTree(3)
	out := slip.Append([]byte{}, obj)
	expect := `(0 1 (0) 2 (0 1 (0))
   3
   (0 1 (0) 2
      (0 1 (0))))`
	tt.Equal(t, expect, string(out))
	// flat
	slip.SetVar(key, nil)
	expect = `(0 1 (0) 2 (0 1 (0)) 3 (0 1 (0) 2 (0 1 (0))))`
	out = slip.Append([]byte{}, obj)
	tt.Equal(t, expect, string(out))
}

func TestPrintRadix(t *testing.T) {
	key := slip.Symbol("*print-radix*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.True, val)

	slip.SetVar(key, nil)
	val, _ = slip.GetVar(key)
	tt.Nil(t, val)

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)
}

func TestPrintReadably(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.True, val)

	tt.Panic(t, func() { _ = slip.Append([]byte{}, (*slip.FileStream)(os.Stdout)) })

	out := slip.Append([]byte{}, (slip.String)("special &"))
	tt.Equal(t, `"special &"`, string(out))

	slip.SetVar(key, nil)
	val, _ = slip.GetVar(key)
	tt.Nil(t, val)

	out = slip.Append([]byte{}, (*slip.FileStream)(os.Stdout))
	tt.Equal(t, "/^#<FILE.*/", string(out))

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)
}

func TestPrintRightMargin(t *testing.T) {
	key := slip.Symbol("*print-right-margin*")
	orig, has := slip.GetVar(key)
	tt.Equal(t, true, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, nil)
	var val slip.Object
	val, _ = slip.GetVar(key)
	tt.Nil(t, val)

	slip.SetVar(key, slip.Fixnum(10))
	val, _ = slip.GetVar(key)
	tt.Equal(t, slip.Fixnum(10), val)

	doc := slip.DescribeVar(key)
	tt.NotEqual(t, "", doc)

	tt.Panic(t, func() { slip.SetVar(key, slip.Fixnum(-10)) })
}

func makeTestTree(n int) slip.Object {
	var list slip.List
	for ; 0 <= n; n-- {
		if 0 < n {
			list = append(slip.List{makeTestTree(n - 1)}, list...)
		}
		list = append(slip.List{slip.Fixnum(n)}, list...)
	}
	return list
}

func TestPrinterWrite(t *testing.T) {
	pr, pw, err := os.Pipe()
	tt.Nil(t, err)
	defer func() { _ = pw.Close(); _ = pr.Close() }()

	key := slip.Symbol("*standard-output*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, (*slip.FileStream)(pw))
	slip.Write(slip.List{nil, slip.True})

	pw.Close()
	var out []byte
	out, err = io.ReadAll(pr)
	tt.Nil(t, err)
	tt.Equal(t, "(nil t)", string(out))

	tt.Panic(t, func() { slip.Write(slip.List{slip.True, nil}) })
}

func TestWarnStandardOutput(t *testing.T) {
	pr, pw, err := os.Pipe()
	tt.Nil(t, err)
	defer func() { _ = pw.Close(); _ = pr.Close() }()

	key := slip.Symbol("*error-output*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, (*slip.FileStream)(pw))

	slip.Warn("duck")

	pw.Close()
	var out []byte
	out, err = io.ReadAll(pr)
	tt.Nil(t, err)
	tt.Equal(t, "Warning: duck\n", string(out))
}

func TestWarnInteractive(t *testing.T) {
	pr, pw, err := os.Pipe()
	tt.Nil(t, err)
	defer func() { _ = pw.Close(); _ = pr.Close() }()

	ansiKey := slip.Symbol("*print-ansi*")
	ansiOrig, _ := slip.GetVar(ansiKey)

	key := slip.Symbol("*standard-output*")
	orig, _ := slip.GetVar(key)

	slip.Interactive = true
	defer func() {
		slip.SetVar(key, orig)
		slip.SetVar(ansiKey, ansiOrig)
		slip.Interactive = false
	}()

	slip.SetVar(key, (*slip.FileStream)(pw))

	slip.SetVar(ansiKey, slip.True)
	slip.Warn("duck")
	slip.SetVar(ansiKey, nil)
	slip.Warn("duck")

	pw.Close()
	var out []byte
	out, err = io.ReadAll(pr)
	tt.Nil(t, err)
	tt.Equal(t, "\x1b[31mWarning: duck\x1b[m\nWarning: duck\n", string(out))
}

func TestPrinterFunky(t *testing.T) {
	funk := slip.NewFunc("car", slip.List{slip.NewFunc("car", slip.List{nil})})
	out := slip.Append([]byte{}, funk)

	prettyKey := slip.Symbol("*print-pretty*")
	prettyOrig, _ := slip.GetVar(prettyKey)
	defer slip.SetVar(prettyKey, prettyOrig)
	slip.SetVar(prettyKey, nil)

	tt.Equal(t, "(car (car nil))", string(out))

	slip.SetVar(prettyKey, nil)
	tt.Equal(t, "(car (car nil))", string(out))
}

func TestAppendDocANSI(t *testing.T) {
	out := slip.AppendDoc(nil, `__bold__ has _italic_.`, 2, 20, true)
	tt.Equal(t, "  \x1b[1mbold\x1b[m has \x1b[4mitalic\x1b[m.", string(out))

	out = slip.AppendDoc(nil, `A longer string that should wrap around.`, 2, 20, false)
	tt.Equal(t, "  A longer string\n  that should wrap\n  around.", string(out))

	out = slip.AppendDoc(nil, "Multiline\nwith new lines\n one\n\ntwo", 2, 20, false)
	tt.Equal(t, "  Multiline with new\n  lines\n  one\n  two", string(out))

	out = slip.AppendDoc(nil, "_missing underscore", 2, 40, true)
	tt.Equal(t, "  \x1b[4mmissing underscore\x1b[m", string(out))

	out = slip.AppendDoc(nil, "one\n\ntwo\n\n", 2, 40, true)
	tt.Equal(t, "  one\n  two\n", string(out))
}
