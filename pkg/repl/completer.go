// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"sort"
	"strings"
)

// Completer provides completion choices given a partial word. Words are
// stored in a slice in sorted order to optimize not only the search for a
// match but for returing a slice of matches.
type Completer struct {
	words []string
	// the rest are for interactions
	lo     int
	hi     int
	index  int
	colCnt int
	target string
}

// Init the instance.
func (c *Completer) Init() {
	c.words = nil
	c.lo = 0
	c.hi = 0
	c.index = 0
	c.colCnt = 0
	c.target = ""
}

// Insert a word. No check is made to see if the word already exists. Used on
// startup.
func (c *Completer) Insert(word string) {
	c.words = append(c.words, word)
}

// Sort previously inserted words. Only need to be called after
// inserting. When a word is added with the Add() function it is inserted in
// the correct order.
func (c *Completer) Sort() {
	sort.Strings(c.words)
}

// Add a word to the completer words.
func (c *Completer) Add(word string) {
	words, _, _ := c.Match(word)
	if words == nil {
		c.words = append(c.words, word)
		sort.Strings(c.words)
	}
}

// Remove a word.
func (c *Completer) Remove(word string) {
	// TBD find first then modify with a copy and shorten
}

// Match looks for a match of the word provided. The internal slice of sorted
// words is returned along with the low and high indices into the word slice
// for matches that begin with the provided word.
func (c *Completer) Match(word string) (words []string, lo, hi int) {
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
