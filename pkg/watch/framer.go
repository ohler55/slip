// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"bytes"
	"fmt"
	"strings"
	"unicode/utf8"

	xterm "golang.org/x/term"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var (
	framerFlavor *flavors.Flavor
)

func init() {
	Pkg.Initialize(nil)
	framerFlavor = flavors.DefFlavor("watch-framer",
		map[string]slip.Object{
			"top":    slip.Fixnum(0),
			"left":   slip.Fixnum(0),
			"border": slip.Symbol(":line"),
		},
		[]string{ClientFlavor().Name()},
		slip.List{
			slip.Symbol(":inittable-instance-variables"),
			slip.Symbol(":gettable-instance-variables"),
			slip.Symbol(":settable-instance-variables"),
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`A framer that... TBD.`),
			},
		},
		&Pkg,
	)
	framerFlavor.DefMethod(":changed", ":after", framerChangedCaller{})
	framerFlavor.DefMethod(":init", ":after", framerInitCaller{})
}

type framerInitCaller struct{}

func (caller framerInitCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	c := self.Any.(*client)
	var (
		top  int
		left int
	)
	if num, ok := self.Get("top").(slip.Fixnum); ok {
		top = int(num)
	}
	if num, ok := self.Get("left").(slip.Fixnum); ok {
		left = int(num)
	}
	w, _, _ := xterm.GetSize(0)
	for i, v := range c.vars {
		setCursor(top+i, left)
		if v.val == slip.Unbound {
			fmt.Printf("\x1b[0K%s: <unbound>", v.sym)
		} else {
			vs := slip.ObjectString(v.val)
			if 0 < w && w < left+len(v.sym)+len(vs)+2 && 0 < w-left-len(v.sym)-5 {
				vs = vs[:w-left-len(v.sym)-5] + "..."
			}
			fmt.Printf("\x1b[0K%s: %s", v.sym, vs)
		}
		setCursor(top+i+1, left)
	}
	drawBorder(top-1, left-2, len(c.vars)+1, self.Get("border"))

	return nil
}

func (caller framerInitCaller) Docs() string {
	return clientInitCaller{}.Docs()
}

type framerChangedCaller struct{}

func (caller framerChangedCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	c := self.Any.(*client)
	var (
		top  int
		left int
	)
	if num, ok := self.Get("top").(slip.Fixnum); ok {
		top = int(num)
	}
	if num, ok := self.Get("left").(slip.Fixnum); ok {
		left = int(num)
	}
	w, _, _ := xterm.GetSize(0)
	for i, v := range c.vars {
		if v.sym == args[0] {
			setCursor(top+i, left)
			if v.val == slip.Unbound {
				fmt.Printf("\x1b[0K%s: <unbound>", v.sym)
			} else {
				vs := slip.ObjectString(v.val)
				if 0 < w && w < left+len(v.sym)+len(vs)+2 && 0 < w-left-len(v.sym)-5 {
					vs = vs[:w-left-len(v.sym)-5] + "..."
				}
				fmt.Printf("\x1b[0K%s: %s", v.sym, vs)
			}
			break
		}
	}
	setCursor(top+len(c.vars), left)
	drawBorder(top-1, left-2, len(c.vars)+1, self.Get("border"))

	return nil
}

func (caller framerChangedCaller) Docs() string {
	return clientChangedCaller{}.Docs()
}

func setCursor(v, h int) {
	if v < 0 {
		v = 0
	}
	if h < 0 {
		h = 0
	}
	_, _ = fmt.Printf("\x1b[%d;%dH", v, h)
}

func drawBorder(top, left, height int, border slip.Object) {
	if border == nil {
		return
	}
	w, _, err := xterm.GetSize(0)
	if err != nil {
		w = 80
	}
	if border == slip.Symbol(":line") {
		if 1 <= top {
			setCursor(top, left)
			line := utf8.AppendRune(nil, '┌')
			line = append(line, bytes.Repeat(utf8.AppendRune(nil, '─'), w-left-1)...)
			fmt.Print(string(line))
		}
		if 1 <= left {
			leftEdge := utf8.AppendRune(nil, '│')
			leftEdge = append(leftEdge, ' ')
			for i := 1; i < height; i++ {
				setCursor(top+i, left)
				fmt.Print(string(leftEdge))
			}
		}
		setCursor(top+height, left)
		line := utf8.AppendRune(nil, '┕')
		line = append(line, bytes.Repeat(utf8.AppendRune(nil, '─'), w-left-1)...)
		fmt.Print(string(line))
	} else if c, ok := border.(slip.Character); ok {
		tline := strings.Repeat(string([]rune{rune(c)}), w-left)
		if 1 <= top {
			setCursor(top, left)
			fmt.Print(tline)
		}
		if 1 <= left {
			leftEdge := utf8.AppendRune(nil, rune(c))
			leftEdge = append(leftEdge, ' ')
			for i := 1; i < height; i++ {
				setCursor(top+i, left)
				fmt.Print(string(leftEdge))
			}
		}
		setCursor(top+height, left)
		fmt.Print(tline)
	}
	setCursor(top+height+1, 1)
}
