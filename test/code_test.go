// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

type codeTest struct {
	src    string
	expect string
	kind   string
	raise  bool
}

func (ct *codeTest) test(t *testing.T, i int) {
	if ct.raise {
		tt.Panic(t, func() { _ = slip.ReadString(ct.src) })
		return
	}
	var code slip.Code
	if i%2 == 0 {
		code = slip.ReadString(ct.src)
	} else {
		code = slip.Read([]byte(ct.src))
	}
	tt.Equal(t, ct.expect, code.String(), i, ": ", ct.src)
	if 0 < len(ct.kind) {
		tt.Equal(t, ct.kind, string(code[0].Hierarchy()[0]))
	}
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

func TestCodeComment(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: "t ; comment", expect: "[t]"},
		{src: `;; comment 1
t ; comment`, expect: "[t]"},
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
		{src: "123456789012345678901234567890", expect: "[123456789012345678901234567890]", kind: "bignum"},
		{src: "123456789012345678901234567890.", expect: "[123456789012345678901234567890]", kind: "bignum"},
	} {
		ct.test(t, i)
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
	code := slip.ReadString("(5)")
	tt.Panic(t, func() { code.Compile() })

	code = slip.ReadString("((x))")
	tt.Panic(t, func() { code.Compile() })

	code = slip.ReadString("((lambda () nil))")
	code.Compile()
	tt.SameType(t, &slip.Dynamic{}, code[0])

	code = slip.ReadString("(defvar code-compile-test 5)")
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
