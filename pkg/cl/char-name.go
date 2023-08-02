// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

var charNames = []slip.String{
	"NUL",                  // 00
	"SOH",                  // 01
	"STX",                  // 02
	"ETX",                  // 03
	"EOT",                  // 04
	"ENQ",                  // 05
	"ACK",                  // 06
	"BEL",                  // 07
	"Backspace",            // 08
	"Tab",                  // 09
	"Newline",              // 0a
	"VT",                   // 0b
	"Page",                 // 0c
	"Return",               // 0d
	"SO",                   // 0e
	"SI",                   // 0f
	"DLE",                  // 10
	"DC1",                  // 11
	"DC2",                  // 12
	"DC3",                  // 13
	"DC4",                  // 14
	"NAK",                  // 15
	"SYN",                  // 16
	"ETB",                  // 17
	"CAN",                  // 18
	"EM",                   // 19
	"SUB",                  // 1a
	"ESC",                  // 1B
	"FS",                   // 1c
	"GS",                   // 1d
	"RS",                   // 1e
	"US",                   // 1f
	"Space",                // 20
	"Exclamation-Mark",     // 21
	"Quotation-Mark",       // 22
	"Number-Sign",          // 23
	"Dollar-Sign",          // 24
	"Percent-Sign",         // 25
	"Ampersand",            // 26
	"Apostrophe",           // 27
	"Left-Parenthesis",     // 28
	"Right-Parenthesis",    // 29
	"Asterisk",             // 2a
	"Plus-Sign",            // 2b
	"Comma",                // 2c
	"Hyphen-Minus",         // 2d
	"Full-Stop",            // 2e
	"Solidus",              // 2f
	"Zero",                 // 30
	"One",                  // 31
	"Two",                  // 32
	"Three",                // 33
	"Four",                 // 34
	"Five",                 // 35
	"Six",                  // 36
	"Seven",                // 37
	"Eight",                // 38
	"Nine",                 // 39
	"Colon",                // 3a
	"Semicolon",            // 3b
	"Less-Than-Sign",       // 3c
	"Equals-Sign",          // 3d
	"Greater-Than-Sign",    // 3e
	"Question-Mark",        // 3f
	"Commercial-At",        // 40
	"Capital-A",            // 41
	"Capital-B",            // 42
	"Capital-C",            // 43
	"Capital-D",            // 44
	"Capital-E",            // 45
	"Capital-F",            // 46
	"Capital-G",            // 47
	"Capital-H",            // 48
	"Capital-I",            // 49
	"Capital-j",            // 4a
	"Capital-K",            // 4b
	"Capital-L",            // 4c
	"Capital-M",            // 4d
	"Capital-N",            // 4e
	"Capital-O",            // 4f
	"Capital-P",            // 50
	"Capital-Q",            // 51
	"Capital-R",            // 52
	"Capital-S",            // 53
	"Capital-T",            // 54
	"Capital-U",            // 55
	"Capital-V",            // 56
	"Capital-W",            // 57
	"Capital-X",            // 58
	"Capital-Y",            // 59
	"Capital-Z",            // 5a
	"Left-Square-Bracket",  // 5b
	"Reverse-Solidus",      // 5c
	"Right-Square-Bracket", // 5d
	"Circumflex-Accent",    // 5e
	"Low-Line",             // 5f
	"Grave-Accent",         // 60
	"Small-A",              // 61
	"Small-B",              // 62
	"Small-C",              // 63
	"Small-D",              // 64
	"Small-E",              // 65
	"Small-F",              // 66
	"Small-G",              // 67
	"Small-H",              // 68
	"Small-I",              // 69
	"Small-J",              // 6a
	"Small-K",              // 6b
	"Small-L",              // 6c
	"Small-M",              // 6d
	"Small-N",              // 6e
	"Small-O",              // 6f
	"Small-P",              // 70
	"Small-Q",              // 71
	"Small-R",              // 72
	"Small-S",              // 73
	"Small-T",              // 74
	"Small-U",              // 75
	"Small-V",              // 76
	"Small-W",              // 77
	"Small-X",              // 78
	"Small-Y",              // 79
	"Small-Z",              // 7a
	"Left-Curly-Bracket",   // 7b
	"Vertical-Line",        // 7c
	"Right-Curly-Bracket",  // 7d
	"Tilde",                // 7E
	"Rubout",               // 7F
}

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CharName{Function: slip.Function{Name: "char-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "char-name",
			Args: []*slip.DocArg{
				{Name: "character", Type: "character"},
			},
			Return: "string",
			Text:   `__char-name__ returns the character name for _character_.`,
			Examples: []string{
				`(char-name (code-char 8)) => "Backspace"`,
				`(char-name #\A) => "Small-A"`,
			},
		}, &slip.CLPkg)
}

// CharName represents the char-name function.
type CharName struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CharName) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.PanicType("character", args[0], "character")
	}
	if c <= 0x7f {
		return charNames[c]
	}
	return slip.String(fmt.Sprintf("#\\%c", rune(c)))
}
