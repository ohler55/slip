// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

type codeTest struct {
	src    string
	expect string
	kind   string
	raise  bool
}

func (ct *codeTest) test(t *testing.T, i int) {
	if ct.raise {
		require.Panics(t, func() { _ = slip.ReadString(ct.src) })
		return
	}
	code := slip.ReadString(ct.src)
	require.Equal(t, ct.expect, code.String(), "%d: %s", i, ct.src)
	if 0 < len(ct.kind) {
		require.Equal(t, ct.kind, string(code[0].Hierarchy()[0]))
	}
}

func TestCodeToken(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: "t", expect: "[t]", kind: "t"},
		{src: "nil", expect: "[nil]"},
		{src: "abc", expect: "[abc]", kind: "symbol"},
		{src: "1a", expect: "[1a]", kind: "symbol"},
		{src: "-a", expect: "[-a]", kind: "symbol"},
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
		{src: "123456789012345678901234567890", expect: "[123456789012345678901234567890]", kind: "bignum"},
		{src: "123456789012345678901234567890.", expect: "[123456789012345678901234567890]", kind: "bignum"},
	} {
		ct.test(t, i)
	}
}

func TestCodeList(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: "()", expect: "[()]", kind: "list"},
		{src: "(abc)", expect: "[(abc)]", kind: "list"},
		{src: "(+ 1 2)", expect: "[(+ 1 2)]", kind: "list"},
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
	} {
		ct.test(t, i)
	}
}

func TestCodeSymbolPipe(t *testing.T) {
	for i, ct := range []*codeTest{
		{src: `|abc|`, expect: `[abc]`, kind: "symbol"},
		{src: `||`, expect: `[||]`, kind: "symbol"},
		{src: `|a\nb|`, expect: `[|a
b|
]`, kind: "symbol"},
		{src: `|\u004a\U0001D122|`, expect: `[j𝄢]`, kind: "symbol"},
		{src: `(|abc|)`, expect: `[(abc)]`, kind: "list"},
		{src: `|abc`, raise: true},
	} {
		ct.test(t, i)
	}
}
