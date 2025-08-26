// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Ansi{Function: slip.Function{Name: "ansi", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "ansi",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "code",
					Type: "keyword",
					Text: `Attribute to add to the ANSI sequence. Valid codes are:
   :reset  :bold  :underline  :blink  :bright  :black  :red  :green  :yellow  :blue  :magenta  :cyan  :white
`,
				},
			},
			Text: `__ansi__ returns a string that is an ANSI sequence.`,
			Examples: []string{
				`(ansi :bold) => "\u001b[1m"`,
			},
		}, &Pkg)
}

// Ansi represents the ansi function.
type Ansi struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Ansi) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	var (
		reset  bool
		color  uint
		bright bool
		mod    uint
	)
	for _, arg := range args {
		sym, ok := arg.(slip.Symbol)
		if !ok || len(sym) < 2 || sym[0] != ':' {
			slip.TypePanic(s, depth, "code", arg, "keyword")
		}
		switch strings.ToLower(string(sym)) {
		case ":reset":
			reset = true
		case ":bold":
			mod |= 0x01
		case ":underline":
			mod |= 0x04
		case ":blink":
			mod |= 0x05
		case ":bright":
			bright = true
		case ":black":
			color = 30
		case ":red":
			color = 31
		case ":green":
			color = 32
		case ":yellow":
			color = 33
		case ":blue":
			color = 34
		case ":magenta":
			color = 35
		case ":cyan":
			color = 36
		case ":white":
			color = 37
		default:
			slip.ErrorPanic(s, depth, "%s is not a valid keyword for the ansi function", sym)
		}
	}
	if reset {
		return slip.String("\x1b[m")
	}
	if bright {
		color += 60
	}
	var seq []byte
	seq = append(seq, '\x1b', '[')
	if color == 0 {
		seq = fmt.Appendf(seq, "%d", mod)
	} else {
		seq = fmt.Appendf(seq, "%d;%d", mod, color)
	}
	seq = append(seq, 'm')

	return slip.String(seq)
}
