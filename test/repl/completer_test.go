// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/pkg/repl"
)

func TestCompleter(t *testing.T) {
	var c repl.Completer

	c.Insert("bcd")
	c.Insert("BBB")
	c.Sort()
	c.Add("abc")
	c.Add("zZz")
	c.Add("ccC")
	c.Add("ddd")
	c.Add("Aaa")
	c.Add("faa")
	c.Add("fab")
	c.Add("fac")
	c.Add("fad")

	c.Remove("ddd")
	c.Remove("zzz")
	c.Remove("aa")

	words, lo, hi := c.Match("dd")
	tt.Equal(t, 0, len(words))
	tt.Equal(t, 0, lo)
	tt.Equal(t, 0, hi)

	words, lo, hi = c.Match("a")
	tt.Equal(t, 0, lo)
	tt.Equal(t, 1, hi)
	tt.NotNil(t, words)

	_, lo, hi = c.Match("b")
	tt.Equal(t, 2, lo)
	tt.Equal(t, 3, hi)

	_, lo, hi = c.Match("bc")
	tt.Equal(t, 3, lo)
	tt.Equal(t, 3, hi)

	_, lo, hi = c.Match("fa")
	tt.Equal(t, 5, lo)
	tt.Equal(t, 8, hi)

	_, lo, hi = c.Match("abc")
	tt.Equal(t, 1, lo)
	tt.Equal(t, 1, hi)

	_, lo, hi = c.Match("fac")
	tt.Equal(t, 7, lo)
	tt.Equal(t, 7, hi)
}
