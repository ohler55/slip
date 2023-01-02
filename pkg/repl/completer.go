// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"sort"
	"strings"
)

type completer struct {
	words []string
	// the rest are for interactions
	lo     int
	hi     int
	index  int
	colCnt int
	target string
}

func (c *completer) init() {
	c.words = nil
	c.lo = 0
	c.hi = 0
	c.index = 0
	c.colCnt = 0
	c.target = ""
}

// Don't check to see if the word already exists. Used oon startup.
func (c *completer) insert(word string) {
	c.words = append(c.words, word)
}

func (c *completer) sort() {
	sort.Strings(c.words)
}

func (c *completer) add(word string) {
	words, _, _ := c.match(word)
	if words == nil {
		c.words = append(c.words, word)
		sort.Strings(c.words)
	}
}

func (c *completer) remove(word string) {
	// TBD find first then modify
}

func (c *completer) match(word string) (words []string, lo, hi int) {
	word = strings.ToLower(word)
	// Since words are not evenly distributed across all characters (heavy on
	// *) a binary search is used to find the match.
	lo = 0
	hi = len(c.words) - 1
	lw := c.words[lo]
	hw := c.words[hi]
	if word < lw || hw < word {
		return nil, 0, 0
	}
	var (
		mid int
		mw  string
	)
	cnt := 0
	// Bracket the match then expand based on the runes in the word.
top:
	for 1 < hi-lo {
		mid = (hi + lo) / 2
		mw = c.words[mid]
		switch {
		case word < mw:
			hw = mw
			hi = mid
		case lw < mw:
			lw = mw
			lo = mid
		default: // ==
			lo = mid
			lw = mw
			hi = mid
			hw = mw
			break top
		}
		cnt++
		if 20 < cnt {
			break
		}
	}
	switch {
	case strings.HasPrefix(lw, word):
		if !strings.HasPrefix(hw, word) {
			hi = lo
		}
	case strings.HasPrefix(hw, word):
		lo = hi
	default:
		return nil, 0, 0
	}
	words = c.words
	for 0 < lo && strings.HasPrefix(c.words[lo-1], word) {
		lo--
	}
	for hi < len(c.words)-1 && strings.HasPrefix(c.words[hi+1], word) {
		hi++
	}
	return
}
