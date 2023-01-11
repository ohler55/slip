// Copyright (c) 2023, Peter Ohler, All rights reserved.

package repl

type runeRange struct {
	first rune
	last  rune
	width int
	subs  []*runeRange
}

// Based on DerivedEastAsianWidth-15.0.0.txt from
// https://www.unicode.org/Public/UCD/latest/ucd/extracted/DerivedEastAsianWidth.txt.
var runeTree = []*runeRange{
	{first: 0x20, last: 0x7e, width: 1},
	{first: 0x00, last: 0x20, width: 0},
	{first: 0x7f, last: 0x10FFFF, width: 1, subs: []*runeRange{
		{first: 0x1100, last: 0x115F, width: 2},
		{first: 0x231A, last: 0x2FEF, width: 2, subs: []*runeRange{
			{first: 0x231C, last: 0x2328, width: 1},
			{first: 0x232B, last: 0x23E8, width: 1},
			{first: 0x23ED, last: 0x23EF, width: 1},
			{first: 0x23F1, last: 0x23F2, width: 1},
			{first: 0x23F4, last: 0x25FC, width: 1},
			{first: 0x25FF, last: 0x2613, width: 1},
			{first: 0x2616, last: 0x2647, width: 1},
			{first: 0x2654, last: 0x267E, width: 1},
			{first: 0x2680, last: 0x2692, width: 1},
			{first: 0x2694, last: 0x26A0, width: 1},
			{first: 0x26A2, last: 0x26A9, width: 1},
			{first: 0x26AC, last: 0x26BC, width: 1},
			{first: 0x26BF, last: 0x26C3, width: 1},
			{first: 0x26C6, last: 0x26CD, width: 1},
			{first: 0x26CF, last: 0x26D3, width: 1},
			{first: 0x26D5, last: 0x26E9, width: 1},
			{first: 0x26EB, last: 0x26F1, width: 1},
			{first: 0x26F4, last: 0x26F4, width: 1},
			{first: 0x26F5, last: 0x26F9, width: 1},
			{first: 0x26FB, last: 0x26FC, width: 1},
			{first: 0x26FE, last: 0x2704, width: 1},
			{first: 0x2706, last: 0x2709, width: 1},
			{first: 0x270C, last: 0x2727, width: 1},
			{first: 0x2729, last: 0x274B, width: 1},
			{first: 0x274D, last: 0x274D, width: 1},
			{first: 0x274F, last: 0x2752, width: 1},
			{first: 0x2756, last: 0x2756, width: 1},
			{first: 0x2758, last: 0x2794, width: 1},
			{first: 0x2798, last: 0x27AF, width: 1},
			{first: 0x27B1, last: 0x27BE, width: 1},
			{first: 0x27C0, last: 0x2B1A, width: 1},
			{first: 0x2B1D, last: 0x2B4F, width: 1},
			{first: 0x2B51, last: 0x2B54, width: 1},
			{first: 0x2B56, last: 0x2E7F, width: 1},
			{first: 0x2E9A, last: 0x2E9A, width: 1},
			{first: 0x2EF4, last: 0x2EFF, width: 1},
			{first: 0x2FD6, last: 0x2FEF, width: 1},
		},
		},
		{first: 0x3000, last: 0xFE6B, width: 2, subs: []*runeRange{
			{first: 0x303F, last: 0x3040, width: 1},
			{first: 0x3097, last: 0x3098, width: 1},
			{first: 0x3100, last: 0x3104, width: 1},
			{first: 0x3130, last: 0x3130, width: 1},
			{first: 0x318F, last: 0x318F, width: 1},
			{first: 0x31E4, last: 0x31EF, width: 1},
			{first: 0x321F, last: 0x321F, width: 1},
			{first: 0x3248, last: 0x324F, width: 1},
			{first: 0x4DC0, last: 0x4DFF, width: 1},
			{first: 0xA48D, last: 0xA48F, width: 1},
			{first: 0xA4C7, last: 0xA95C, width: 1},
			{first: 0xA97D, last: 0xABFF, width: 1},
			{first: 0xD7A4, last: 0xF8FF, width: 1},
			{first: 0xFA6E, last: 0xFA6F, width: 1},
			{first: 0xFADA, last: 0xFE0F, width: 1},
			{first: 0xFE1A, last: 0xFE2F, width: 1},
			{first: 0xFE53, last: 0xFE53, width: 1},
			{first: 0xFE67, last: 0xFE67, width: 1},
		},
		},
		{first: 0xFF01, last: 0xFFE6, width: 2, subs: []*runeRange{
			{first: 0xFF61, last: 0xFFDF, width: 1},
		},
		},
		{first: 0x16FE0, last: 0x1F3F5, width: 2, subs: []*runeRange{
			{first: 0x16FE5, last: 0x16FEF, width: 1},
			{first: 0x16FF2, last: 0x16FFF, width: 1},
			{first: 0x187F8, last: 0x187FF, width: 1},
			{first: 0x18CD6, last: 0x18CFF, width: 1},
			{first: 0x18D09, last: 0x1AFEF, width: 1},
			{first: 0x1AFF4, last: 0x1AFF4, width: 1},
			{first: 0x1AFFC, last: 0x1AFFC, width: 1},
			{first: 0x1AFFF, last: 0x1AFFF, width: 1},
			{first: 0x1B123, last: 0x1B131, width: 1},
			{first: 0x1B153, last: 0x1B154, width: 1},
			{first: 0x1B156, last: 0x1B163, width: 1},
			{first: 0x1B168, last: 0x1B169, width: 1},
			{first: 0x182FC, last: 0x1F003, width: 1},
			{first: 0x1F005, last: 0x1F0CE, width: 1},
			{first: 0x1F0D0, last: 0x1F18D, width: 1},
			{first: 0x1F18F, last: 0x1F190, width: 1},
			{first: 0x1F19B, last: 0x1F1FF, width: 1},
			{first: 0x1F203, last: 0x1F20F, width: 1},
			{first: 0x1F239, last: 0x1F239, width: 1},
			{first: 0x1F249, last: 0x1F249, width: 1},
			{first: 0x1F252, last: 0x1F25F, width: 1},
			{first: 0x1F266, last: 0x1F2FF, width: 1},
			{first: 0x1F321, last: 0x1F32C, width: 1},
			{first: 0x1F336, last: 0x1F336, width: 1},
			{first: 0x1F37D, last: 0x1F37D, width: 1},
			{first: 0x1F394, last: 0x1F39F, width: 1},
			{first: 0x1F3CB, last: 0x1F3CE, width: 1},
			{first: 0x1F3D4, last: 0x1F3DF, width: 1},
			{first: 0x1F3F1, last: 0x1f3F3, width: 1},
		},
		},
		{first: 0x1F440, last: 0x1FAF8, width: 2, subs: []*runeRange{
			{first: 0x1F441, last: 0x1F441, width: 1},
			{first: 0x1F4FD, last: 0x1F4FE, width: 1},
			{first: 0x1F53E, last: 0x1F54F, width: 1},
			{first: 0x1F568, last: 0x1F579, width: 1},
			{first: 0x1F57B, last: 0x1F594, width: 1},
			{first: 0x1F597, last: 0x1F5A3, width: 1},
			{first: 0x1F5A5, last: 0x1F5FA, width: 1},
			{first: 0x1F650, last: 0x1F67F, width: 1},
			{first: 0x1F6C6, last: 0x1F6CB, width: 1},
			{first: 0x1F6CD, last: 0x1F6CF, width: 1},
			{first: 0x1F6D3, last: 0x1F6D4, width: 1},
			{first: 0x1F6D8, last: 0x1F6DB, width: 1},
			{first: 0x1F6E0, last: 0x1F6EA, width: 1},
			{first: 0x1F6ED, last: 0x1F6F3, width: 1},
			{first: 0x1F6FD, last: 0x1F7DF, width: 1},
			{first: 0x1F7EC, last: 0x1F7EF, width: 1},
			{first: 0x1F7F1, last: 0x1F90B, width: 1},
			{first: 0x1F93B, last: 0x1F93B, width: 1},
			{first: 0x1F946, last: 0x1F946, width: 1},
			{first: 0x1FA00, last: 0x1FA6F, width: 1},
			{first: 0x1FA7D, last: 0x1FA7F, width: 1},
			{first: 0x1FA89, last: 0x1FA89, width: 1},
			{first: 0x1FABE, last: 0x1FABE, width: 1},
			{first: 0x1FAC6, last: 0x1FACD, width: 1},
			{first: 0x1FADC, last: 0x1FADF, width: 1},
			{first: 0x1FAE9, last: 0x1FAEF, width: 1},
		},
		},
		{first: 0x20000, last: 0x2FFFD, width: 2},
		{first: 0x30000, last: 0x3FFFD, width: 2},
	},
	},
}

// RuneWidth returns the display width of a rune,
func RuneWidth(r rune) int {
	w := 1
	subs := runeTree
top:
	for _, rr := range subs {
		if rr.first <= r && r <= rr.last {
			w = rr.width
			subs = rr.subs
			goto top
		}
	}
	return w
}
