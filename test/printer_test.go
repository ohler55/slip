// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

func TestPrintANSI(t *testing.T) {
	key := slip.Symbol("*print-ansi*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.NotNil(t, val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)
}

func TestPrintArray(t *testing.T) {
	key := slip.Symbol("*print-array*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.True, val)

	a0 := slip.NewArray(slip.Fixnum(0))
	a1 := slip.NewArray(slip.Fixnum(1), 3)
	a := testArray()
	out := slip.Append([]byte{}, a0)
	require.Equal(t, "#0A()", string(out))
	out = slip.Append([]byte{}, a1)
	require.Equal(t, "#(1 1 1)", string(out))
	out = slip.Append([]byte{}, a)
	require.Equal(t, "#3A(((0 1 2 3) (4 5 6 7) (8 9 10 11)) ((12 13 14 15) (16 17 18 19) (20 21 22 23)))", string(out))

	slip.SetVar(key, nil)
	val, _ = slip.GetVar(key)
	require.Nil(t, val)

	out = slip.Append([]byte{}, a0)
	require.Equal(t, "#<(ARRAY T NIL)>", string(out))
	out = slip.Append([]byte{}, a1)
	require.Equal(t, "#<(VECTOR 3)>", string(out))
	out = slip.Append([]byte{}, a)
	require.Equal(t, "#<(ARRAY T (2 3 4))>", string(out))

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)
}

func TestPrintBase(t *testing.T) {
	key := slip.Symbol("*print-base*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.Fixnum(16))
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.Fixnum(16), val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	require.Panics(t, func() { slip.SetVar(key, slip.True) })

	radixKey := slip.Symbol("*print-radix*")
	origRadix, _ := slip.GetVar(radixKey)
	defer slip.SetVar(radixKey, origRadix)
	slip.SetVar(radixKey, slip.True)

	obj := slip.Fixnum(37)
	for _, pair := range []*struct {
		base   int
		expect string
	}{
		{base: 2, expect: "#b100101"},
		{base: 3, expect: "#3r1101"},
		{base: 8, expect: "#o45"},
		{base: 10, expect: "37."},
		{base: 16, expect: "#x25"},
	} {
		slip.SetVar(key, slip.Fixnum(pair.base))
		require.Equal(t, pair.expect, string(slip.Append([]byte{}, obj)), "%d: printer append", pair.base)
		require.Equal(t, pair.expect, obj.String(), "base %d obj.Append()", pair.base)
	}
}

func TestPrintCase(t *testing.T) {
	key := slip.Symbol("*print-case*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.Symbol(":downcase"))
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.Symbol(":downcase"), val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	require.Panics(t, func() { slip.SetVar(key, slip.True) })

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
		require.Equal(t, pair.expect, string(slip.Append([]byte{}, obj)), "%s: printer append", pair.sym)
		require.Equal(t, pair.expect, obj.String(), "case %s obj.Append()", pair.sym)
	}
}

func TestPrintCircle(t *testing.T) {
	key := slip.Symbol("*print-circle*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.True, val)

	slip.SetVar(key, nil)
	val, _ = slip.GetVar(key)
	require.Nil(t, val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)
}

func TestPrintEscape(t *testing.T) {
	key := slip.Symbol("*print-escape*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, nil)
	val, _ := slip.GetVar(key)
	require.Nil(t, val)

	obj := slip.Character('X')
	require.Equal(t, "X", string(slip.Append([]byte{}, obj)))

	slip.SetVar(key, slip.True)
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.True, val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	obj = slip.Character('X')
	require.Equal(t, "#\\X", string(slip.Append([]byte{}, obj)))
}

func TestPrintGensym(t *testing.T) {
	key := slip.Symbol("*print-gensym*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.True, val)

	slip.SetVar(key, nil)
	val, _ = slip.GetVar(key)
	require.Nil(t, val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)
}

func TestPrintLength(t *testing.T) {
	key := slip.Symbol("*print-length*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, nil)
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.Nil(t, val)

	var list slip.List
	for i := 10; 0 <= i; i-- {
		list = append(list, slip.Fixnum(i))
	}
	out := slip.Append([]byte{}, list)
	require.Equal(t, "(0 1 2 3 4 5 6 7 8 9 10)", string(out))

	slip.SetVar(key, slip.Fixnum(10))
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.Fixnum(10), val)

	out = slip.Append([]byte{}, list)
	require.Equal(t, "(0 1 2 3 4 5 6 7 8 9 ...)", string(out))

	prettyKey := slip.Symbol("*print-pretty*")
	prettyOrig, _ := slip.GetVar(prettyKey)
	defer slip.SetVar(prettyKey, prettyOrig)
	slip.SetVar(prettyKey, nil)

	out = slip.Append([]byte{}, list)
	require.Equal(t, "(0 1 2 3 4 5 6 7 8 9 ...)", string(out))

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	require.Panics(t, func() { slip.SetVar(key, slip.Fixnum(-10)) })
}

func TestPrintLevel(t *testing.T) {
	key := slip.Symbol("*print-level*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, nil)
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.Nil(t, val)

	var list slip.List
	for i := 7; 0 <= i; i-- {
		list = slip.List{list, slip.Fixnum(i)}
	}
	out := slip.Append([]byte{}, list)
	require.Equal(t, "(0 (1 (2 (3 (4 (5 (6 (7 ()))))))))", string(out))

	slip.SetVar(key, slip.Fixnum(6))
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.Fixnum(6), val)

	out = slip.Append([]byte{}, list)
	require.Equal(t, "(0 (1 (2 (3 (4 (5 #))))))", string(out))

	prettyKey := slip.Symbol("*print-pretty*")
	prettyOrig, _ := slip.GetVar(prettyKey)
	defer slip.SetVar(prettyKey, prettyOrig)
	slip.SetVar(prettyKey, nil)

	out = slip.Append([]byte{}, list)
	require.Equal(t, "(0 (1 (2 (3 (4 (5 #))))))", string(out))

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	require.Panics(t, func() { slip.SetVar(key, slip.Fixnum(-10)) })
}

func TestPrintLines(t *testing.T) {
	key := slip.Symbol("*print-lines*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, nil)
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.Nil(t, val)

	widthKey := slip.Symbol("*print-right-margin*")
	origWidth, _ := slip.GetVar(widthKey)
	defer slip.SetVar(widthKey, origWidth)
	slip.SetVar(widthKey, slip.Fixnum(10))

	var list slip.List
	for i := 7; 0 <= i; i-- {
		list = slip.List{list, slip.Fixnum(i)}
	}
	out := slip.Append([]byte{}, list)
	require.Equal(t, `(0
 (1
  (2
   (3
    (4
     (5
      (6
       (7
        ()))))))))
`, string(out))

	slip.SetVar(key, slip.Fixnum(6))
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.Fixnum(6), val)

	out = slip.Append([]byte{}, list)
	require.Equal(t, `(0
 (1
  (2
   (3
    (4
     (5 ..))))))
`, string(out))

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	require.Panics(t, func() { slip.SetVar(key, slip.Fixnum(-10)) })
}

func TestPrintMiserWidth(t *testing.T) {
	key := slip.Symbol("*print-miser-width*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.Fixnum(10))
	val, _ := slip.GetVar(key)
	require.Equal(t, slip.Fixnum(10), val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	require.Panics(t, func() { slip.SetVar(key, slip.Fixnum(-10)) })
}

func TestPrintPretty(t *testing.T) {
	key := slip.Symbol("*print-pretty*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.True, val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	widthKey := slip.Symbol("*print-right-margin*")
	origWidth, _ := slip.GetVar(widthKey)
	defer slip.SetVar(widthKey, origWidth)
	slip.SetVar(widthKey, slip.Fixnum(20))

	obj := makeTestTree(3)
	out := slip.Append([]byte{}, obj)
	expect := `(0 1 (0) 2 (0 1 (0))
   3
   (0 1 (0) 2
      (0 1 (0))))
`
	require.Equal(t, expect, string(out))
	// flat
	slip.SetVar(key, nil)
	expect = `(0 1 (0) 2 (0 1 (0)) 3 (0 1 (0) 2 (0 1 (0))))`
	out = slip.Append([]byte{}, obj)
	require.Equal(t, expect, string(out))
}

func TestPrintRadix(t *testing.T) {
	key := slip.Symbol("*print-radix*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.True, val)

	slip.SetVar(key, nil)
	val, _ = slip.GetVar(key)
	require.Nil(t, val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)
}

func TestPrintReadably(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.True, val)

	require.Panics(t, func() { _ = slip.Append([]byte{}, (*slip.FileStream)(os.Stdout)) })

	slip.SetVar(key, nil)
	val, _ = slip.GetVar(key)
	require.Nil(t, val)

	out := slip.Append([]byte{}, (*slip.FileStream)(os.Stdout))
	require.Regexp(t, "^#<FILE.*", string(out))

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)
}

func TestPrintRightMargin(t *testing.T) {
	key := slip.Symbol("*print-right-margin*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, nil)
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.Nil(t, val)

	slip.SetVar(key, slip.Fixnum(10))
	val, _ = slip.GetVar(key)
	require.Equal(t, slip.Fixnum(10), val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	require.Panics(t, func() { slip.SetVar(key, slip.Fixnum(-10)) })
}

func makeTestTree(n int) slip.Object {
	var list slip.List
	for ; 0 <= n; n-- {
		if 0 < n {
			list = append(list, makeTestTree(n-1))
		}
		list = append(list, slip.Fixnum(n))
	}
	return list
}

func TestPrinterWrite(t *testing.T) {
	pr, pw, err := os.Pipe()
	require.NoError(t, err)
	defer func() { _ = pw.Close(); _ = pr.Close() }()

	key := slip.Symbol("*standard-output*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, (*slip.FileStream)(pw))
	slip.Write(slip.List{slip.True, nil})

	pw.Close()
	var out []byte
	out, err = ioutil.ReadAll(pr)
	require.NoError(t, err)
	require.Equal(t, "(nil t)", string(out))

	require.Panics(t, func() { slip.Write(slip.List{slip.True, nil}) })
}

func TestWarningStandardOutput(t *testing.T) {
	pr, pw, err := os.Pipe()
	require.NoError(t, err)
	defer func() { _ = pw.Close(); _ = pr.Close() }()

	key := slip.Symbol("*error-output*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, (*slip.FileStream)(pw))

	slip.Warning("duck")

	pw.Close()
	var out []byte
	out, err = ioutil.ReadAll(pr)
	require.NoError(t, err)
	require.Equal(t, "Warning: duck\n", string(out))
}

func TestWarningInteractive(t *testing.T) {
	pr, pw, err := os.Pipe()
	require.NoError(t, err)
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
	slip.Warning("duck")
	slip.SetVar(ansiKey, nil)
	slip.Warning("duck")

	pw.Close()
	var out []byte
	out, err = ioutil.ReadAll(pr)
	require.NoError(t, err)
	require.Equal(t, "\x1b[31mWarning: duck\x1b[m\nWarning: duck\n", string(out))
}

func TestPrinterFunky(t *testing.T) {
	funk := slip.NewFunc("car", slip.List{slip.NewFunc("car", slip.List{nil})})
	out := slip.Append([]byte{}, funk)

	prettyKey := slip.Symbol("*print-pretty*")
	prettyOrig, _ := slip.GetVar(prettyKey)
	defer slip.SetVar(prettyKey, prettyOrig)
	slip.SetVar(prettyKey, nil)

	require.Equal(t, "(car (car nil))", string(out))

	slip.SetVar(prettyKey, nil)
	require.Equal(t, "(car (car nil))", string(out))
}
