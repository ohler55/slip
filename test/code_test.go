// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

type codeTest struct {
	src    string
	expect string
	kind   string
	raise  bool
}

func (ct *codeTest) test(t *testing.T, i int) {
	scope := slip.NewScope()
	if ct.raise {
		tt.Panic(t, func() { _ = slip.ReadString(ct.src, scope) })
		return
	}
	var code slip.Code
	if i%2 == 0 {
		code = slip.ReadString(ct.src, scope)
	} else {
		code = slip.Read([]byte(ct.src), scope)
	}
	if 0 < len(ct.kind) {
		tt.Equal(t, ct.kind, string(code[0].Hierarchy()[0]))
	}
	tt.Equal(t, ct.expect, code.String(), i, ": ", ct.src)
}

func TestCodeToken(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: "t", expect: "[t]", kind: "t"},
		{src: "nil", expect: "[nil]"},
		{src: "abc", expect: "[abc]", kind: "symbol"},
		{src: "1a", expect: "[1a]", kind: "symbol"},
		{src: "1e2e3", expect: "[1e2e3]", kind: "symbol"},
		{src: "-a", expect: "[-a]", kind: "symbol"},
		{src: ":abc", expect: "[:abc]", kind: "symbol"},
		{src: "cl:abc", expect: "[cl:abc]", kind: "symbol"},
		{src: "\nt\n", expect: "[t]", kind: "t"},
		{src: "@2022-04-10T18:52:17Z", expect: "[@2022-04-10T18:52:17Z]", kind: "time"},
	} {
		ct.test(t, i)
	}
}

func TestCodeReadOne(t *testing.T) {
	code, pos := slip.ReadOne([]byte("abc def"), slip.NewScope())
	tt.Equal(t, slip.Symbol("abc"), code[0])
	tt.Equal(t, 3, pos)
}

func TestCodeComment(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: "t ; comment", expect: "[t]"},
		{src: `;; comment 1
t ; comment`, expect: "[t]"},
	} {
		ct.test(t, i)
	}
}

func TestCodeBlockComment(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: "(a #| b |# c)", expect: "[(a c)]"},
		{src: `t
#|
"abc"
|# nil`, expect: "[t nil]"},
	} {
		ct.test(t, i)
	}
}

func TestCodeNumber(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: "123", expect: "[123]", kind: "fixnum"},
		{src: "123.", expect: "[123]", kind: "fixnum"},
		{src: "-123.", expect: "[-123]", kind: "fixnum"},
		{src: "1.23e-1", expect: "[0.123]", kind: "double-float"},
		{src: "1.23s-1", expect: "[0.123]", kind: "single-float"},
		{src: "1.23f-1", expect: "[0.123]", kind: "single-float"},
		{src: "1.23d-1", expect: "[0.123]", kind: "double-float"},
		{src: "5d-1", expect: "[0.5]", kind: "double-float"},
		{src: "1.23l-1", expect: "[0.123]", kind: "long-float"},
		{src: "0.1L+0", expect: "[0.1]", kind: "long-float"},
		{src: "123456789012345678901234567890", expect: "[123456789012345678901234567890]", kind: "bignum"},
		{src: "123456789012345678901234567890.", expect: "[123456789012345678901234567890]", kind: "bignum"},
	} {
		ct.test(t, i)
	}
}

func TestCodeBaseFixnum(t *testing.T) {
	scope := slip.NewScope()
	for i, td := range []*struct {
		base   int
		src    string
		expect int
	}{
		{base: 2, src: "1010", expect: 10},
		{base: 3, src: "210", expect: 21},
		{base: 4, src: "321", expect: 57},
		{base: 5, src: "401", expect: 101},
		{base: 6, src: "55", expect: 35},
		{base: 7, src: "666", expect: 342},
		{base: 8, src: "073", expect: 59},
		{base: 9, src: "88", expect: 80},
		{base: 10, src: "975", expect: 975},
		{base: 11, src: "a1", expect: 111},
		{base: 12, src: "ba", expect: 142},
		{base: 13, src: "cb", expect: 167},
		{base: 14, src: "dc", expect: 194},
		{base: 15, src: "ed", expect: 223},
		{base: 16, src: "fe", expect: 254},
		{base: 36, src: "zz", expect: 1295},
	} {
		scope.Let("*read-base*", slip.Fixnum(td.base))
		code := slip.ReadString(td.src, scope)
		tt.Equal(t, slip.FixnumSymbol, code[0].Hierarchy()[0])
		tt.Equal(t, slip.Fixnum(td.expect), code[0], i, ": ", td.src)
	}
}

func TestCodeRatio(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: "1/2", expect: "[1/2]", kind: "ratio"},
		{src: "-1/2", expect: "[-1/2]", kind: "ratio"},
		{src: "1/-2", expect: "[|1/-2|]", kind: "symbol"},
	} {
		ct.test(t, i)
	}
}

func TestCodeBaseRatio(t *testing.T) {
	scope := slip.NewScope()
	for i, td := range []*struct {
		base   int
		src    string
		expect string
	}{
		{base: 2, src: "01/10", expect: "1/2"},
		{base: 3, src: "21/12", expect: "7/5"},
		{base: 4, src: "32/23", expect: "14/11"},
		{base: 5, src: "43/34", expect: "23/19"},
		{base: 6, src: "54/45", expect: "34/29"},
		{base: 7, src: "65/56", expect: "47/41"},
		{base: 8, src: "76/67", expect: "62/55"},
		{base: 9, src: "87/78", expect: "79/71"},
		{base: 10, src: "98/89", expect: "98/89"},
		{base: 11, src: "a9/9a", expect: "119/109"},
		{base: 12, src: "ba/ab", expect: "142/131"},
		{base: 13, src: "cb/bc", expect: "167/155"},
		{base: 14, src: "dc/cd", expect: "194/181"},
		{base: 15, src: "ed/de", expect: "223/209"},
		{base: 16, src: "fe/ef", expect: "254/239"},
		{base: 36, src: "zy/yz", expect: "1294/1259"},
	} {
		scope.Let("*read-base*", slip.Fixnum(td.base))
		code := slip.ReadString(td.src, scope)
		tt.Equal(t, slip.RatioSymbol, code[0].Hierarchy()[0])
		tt.Equal(t, td.expect, code[0].String(), i, ": ", td.src)
	}
}

func TestCodeComplex(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: "#c(1 2)", expect: "[#C(1 2)]", kind: "complex"},
		{src: "#c(-3 2)", expect: "[#C(-3 2)]", kind: "complex"},
		{src: "#c(-3 -2)", expect: "[#C(-3 -2)]", kind: "complex"},
		{src: "#c(-3)", raise: true},
		{src: "#c(1 2 3)", raise: true},
		{src: "#c(t 2)", raise: true},
		{src: "#c(1 t)", raise: true},
	} {
		ct.test(t, i)
	}
}

func TestCodeList(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: "()", expect: "[nil]", kind: "list"},
		{src: "(abc)", expect: "[(abc)]", kind: "list"},
		{src: "(+ 1 2)", expect: "[(+ 1 2)]", kind: "list"},
		{src: ")", raise: true},
		{src: "(+ 1", raise: true},
	} {
		ct.test(t, i)
	}
}

func TestCodeCons(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: "(a . b)", expect: "[(a . b)]", kind: "cons"},
	} {
		ct.test(t, i)
	}
}

func TestCodeString(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: `""`, expect: `[""]`, kind: "string"},
		{src: `"abc"`, expect: `["abc"]`, kind: "string"},
		{src: `"a\nb"`, expect: `["a
b"]`, kind: "string"},
		{src: `"\u004a\U0001D122"`, expect: `["J𝄢"]`, kind: "string"},
		{src: `("abc")`, expect: `[("abc")]`, kind: "list"},
		{src: `"abc`, raise: true},
		{src: `"\u004"`, raise: true},
		{src: `"\u004`, raise: true},
		{src: `"\z`, raise: true},
		{src: `"\`, raise: true},
	} {
		ct.test(t, i)
	}
}

func TestCodeSymbolPipe(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: `|abc|`, expect: `[abc]`, kind: "symbol"},
		{src: `||`, expect: `[||]`, kind: "symbol"},
		{src: `|a\nb|`, expect: `[|a
b|]`, kind: "symbol"},
		{src: `|\u004a\U0001D122|`, expect: `[j𝄢]`, kind: "symbol"},
		{src: `(|abc|)`, expect: `[(abc)]`, kind: "list"},
		{src: `|abc`, raise: true},
	} {
		ct.test(t, i)
	}
}

func TestCodeCharacter(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: `#\A`, expect: `[#\A]`, kind: "character"},
		{src: `#\A `, expect: `[#\A]`, kind: "character"},
		{src: `#\u004d`, expect: `[#\M]`, kind: "character"},
		{src: `#\SPACE`, expect: `[#\Space]`, kind: "character"},
		{src: `#\,`, expect: `[#\,]`, kind: "character"},
		{src: `#\ぴ`, expect: `[#\ぴ]`, kind: "character"},
		{src: `#\u00`, raise: true},
		{src: `#z`, raise: true},
		{src: `#\`, raise: true},
		{src: `#\u00000000`, raise: true},
		{src: `(#\A #\B) `, expect: `[(#\A #\B)]`, kind: "list"},
	} {
		ct.test(t, i)
	}
}

func TestCodeVector(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: `#()`, expect: `[#()]`, kind: "vector"},
		{src: `#(1)`, expect: `[#(1)]`, kind: "vector"},
		{src: `#(1 2 3)`, expect: `[#(1 2 3)]`, kind: "vector"},
	} {
		ct.test(t, i)
	}
}

func TestCodeBinary(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: `#b0`, expect: `[0]`, kind: "fixnum"},
		{src: `#b1`, expect: `[1]`, kind: "fixnum"},
		{src: `#b1010`, expect: `[10]`, kind: "fixnum"},
		{src: `#b1010 `, expect: `[10]`, kind: "fixnum"},
		{
			src:    `#b1010101010101010101010101010101010101010101010101010101010101010`,
			expect: `[12297829382473034410]`,
			kind:   "bignum",
		},
		{src: `#x100000000000000000`, expect: `[295147905179352825856]`, kind: "bignum"},
		{src: `#b012`, raise: true},
		{src: `#b`, raise: true},
	} {
		ct.test(t, i)
	}
}

func TestCodeOct(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: `#o0`, expect: `[0]`, kind: "fixnum"},
		{src: `#o1`, expect: `[1]`, kind: "fixnum"},
		{src: `#o1234`, expect: `[668]`, kind: "fixnum"},
		{src: `#o1234 `, expect: `[668]`, kind: "fixnum"},
		{src: `#o123456701234567012345670`, expect: `[770996035962450594744]`, kind: "bignum"},
		{src: `#o012345678`, raise: true},
	} {
		ct.test(t, i)
	}
}

func TestCodeHex(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: `#x0`, expect: `[0]`, kind: "fixnum"},
		{src: `#x1`, expect: `[1]`, kind: "fixnum"},
		{src: `#x1a2B`, expect: `[6699]`, kind: "fixnum"},
		{src: `#x1a2B `, expect: `[6699]`, kind: "fixnum"},
		{src: `#x123456789abcdef123456789abcdef`, expect: `[94522879700260683142460330790866415]`, kind: "bignum"},
		{src: `#x0123456789abcdefg`, raise: true},
		{src: `(#x1 #x2)`, expect: `[(1 2)]`, kind: "list"},
	} {
		ct.test(t, i)
	}
}

func TestCodeRadix(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: `#3r0`, expect: `[0]`, kind: "fixnum"},
		{src: `#7r1`, expect: `[1]`, kind: "fixnum"},
		{src: `#5r100`, expect: `[25]`, kind: "fixnum"},
		{src: `#33rabcdefghijklmnopqrstuvw`, expect: `[26425650874257907358648568190381955]`, kind: "bignum"},
		{src: `#3r1234`, raise: true},
		{src: `#3r|`, raise: true},
	} {
		ct.test(t, i)
	}
}

func TestCodeArray(t *testing.T) {
	key := slip.Symbol("*print-array*")
	orig, _ := slip.GetVar(key)
	defer slip.SetVar(key, orig)
	slip.SetVar(key, slip.True)

	for i, ct := range []*codeTest{
		{src: `#2A((1 2 3)(4 5 6))`, expect: `[#2A((1 2 3) (4 5 6))]`, kind: "array"},
		{src: `#1A(1 2 3)`, expect: `[#(1 2 3)]`, kind: "vector"},
		{src: `#0A()`, expect: `[#0Anil]`, kind: "array"},
		{src: `#1025A()`, raise: true},
		{src: `#2A((1 2) t)`, raise: true},
	} {
		ct.test(t, i)
	}
}

func TestCodeQuote(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: `#'car`, expect: `[#'car]`, kind: "macro"},
		{src: `(list #'car)`, expect: `[(list (name car))]`, kind: "list"},
		{src: `'abc`, expect: `['abc]`, kind: "macro"},
		{src: `('abc)`, expect: `[('abc)]`, kind: "list"},
		{src: `#'(lambda () nil)`, expect: `[#'(lambda () nil)]`, kind: "macro"},
	} {
		ct.test(t, i)
	}
}

// Checks comma as well since they are used together.
func TestCodeBackquote(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: "`(a ,b)", expect: "[`(a ,b)]"},
		{src: ",b", raise: true},
	} {
		ct.test(t, i)
	}
}

func TestCodeCompile(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(5)", scope)
	tt.Panic(t, func() { code.Compile() })

	code = slip.ReadString("((x))", scope)
	tt.Panic(t, func() { code.Compile() })

	code = slip.ReadString("((lambda () nil))", scope)
	code.Compile()
	tt.SameType(t, &slip.Dynamic{}, code[0])

	code = slip.ReadString("(defvar code-compile-test 5)", scope)
	code.Compile()
	val, has := slip.CurrentPackage.Get("code-compile-test")
	tt.Equal(t, slip.Fixnum(5), val)
	tt.Equal(t, true, has)

	// TBD defparameter
}

func TestCodeStringer(t *testing.T) {
	code := slip.Code{nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
		nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil}
	tt.Equal(t, `[
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
 nil
]`, code.String())
}

func TestCodeReadStreamOk(t *testing.T) {
	sr := strings.NewReader("(+ 1 2 3)")
	code, _ := slip.ReadStream(sr, slip.NewScope())
	tt.Equal(t, 1, len(code))
	tt.Equal(t, "(+ 1 2 3)", slip.ObjectString(code[0]))
}

func TestCodeReadStreamOne(t *testing.T) {
	sr := strings.NewReader("(+ 1 2 3) (- 3 2 1)")
	code, _ := slip.ReadStream(sr, slip.NewScope(), true)
	tt.Equal(t, 1, len(code))
	tt.Equal(t, "(+ 1 2 3)", slip.ObjectString(code[0]))
}

type badReader int

func (w badReader) Read([]byte) (int, error) {
	return 0, fmt.Errorf("oops")
}

func TestCodeReadStreamFail(t *testing.T) {
	tt.Panic(t, func() { _, _ = slip.ReadStream(badReader(0), slip.NewScope()) })
}

func TestCodeReadStreamEachOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (quux)
                  (read-each (make-string-input-stream "(1 2 3)") (lambda (x) (setq quux x)))
                  quux)`,
		Expect: "(1 2 3)",
	}).Test(t)
}

func TestCodeReadStreamPushOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((chan (make-channel 2)))
                  (read-push (make-string-input-stream "(1 2 3)") chan)
                  (channel-pop chan))`,
		Expect: "(1 2 3)",
	}).Test(t)
}
