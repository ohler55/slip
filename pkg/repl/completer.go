// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

// Note that this is not thread safe when coupled with a completer. If a word
// is added or removed while the user is interacting with a Completer instance
// the lo, hi, and index could become offset.
var completerWords []string

// Completer provides completion choices given a partial word. Words are
// stored in a slice in sorted order to optimize not only the search for a
// match but for returing a slice of matches.
type Completer struct {
	lo     int
	hi     int
	index  int
	colCnt int
	target string
}

func initWords() {
	completerWords = nil
	insert := func(word string) {
		word = strings.ToLower(word)
		completerWords = append(completerWords, word)
	}
	slip.CurrentPackage.EachFuncName(insert)
	slip.CurrentPackage.EachVarName(insert)
	for name := range slip.Constants {
		insert(name)
	}
	sort.Strings(completerWords)
}

// Init the instance.
func (c *Completer) Init() {
	c.lo = 0
	c.hi = 0
	c.index = 0
	c.colCnt = 0
	c.target = ""
}

func setHook(p *slip.Package, key string) {
	if p == &Pkg ||
		strings.HasPrefix(key, "*print-") ||
		key == "*bag-time-format*" ||
		key == "*bag-time-wrap*" {
		modifiedVars[key] = true
		updateConfigFile()
	}
	if len(completerWords) == 0 {
		initWords()
	}
	addWord(key)
}

func unsetHook(p *slip.Package, key string) {
	removeWord(key)
}

func defunHook(p *slip.Package, key string) {
	if len(completerWords) == 0 {
		initWords()
	}
	addWord(key)
}

// WordMatch looks for a match of the word provided. The internal slice of sorted
// words is returned along with the low and high indices into the word slice
// for matches that begin with the provided word.
func WordMatch(word string) (words []string, lo, hi int) {
	if len(completerWords) == 0 {
		initWords()
	}
	word = strings.ToLower(word)
	// Since words are not evenly distributed across all characters (heavy on
	// *) a binary search is used to find the match.
	lo = 0
	hi = len(completerWords) - 1
	lw := completerWords[lo]
	hw := completerWords[hi]
	if (word < lw && !strings.HasPrefix(lw, word)) || hw < word {
		return nil, 0, 0
	}
	var (
		mid int
		mw  string
	)
	// Bracket the match then expand based on the runes in the word.
	for 1 < hi-lo {
		mid = (hi + lo) / 2
		mw = completerWords[mid]
		switch {
		case word < mw:
			hw = mw
			hi = mid
		case lw < mw:
			lw = mw
			lo = mid
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
	words = completerWords
	// A partial match or full match will always be the lowest lo.
	for hi < len(completerWords)-1 && strings.HasPrefix(completerWords[hi+1], word) {
		hi++
	}
	return
}

func addWord(word string) {
	if len(completerWords) == 0 {
		initWords()
	}
	word = strings.ToLower(word)
	words, _, _ := WordMatch(word)
	if words == nil {
		completerWords = append(completerWords, word)
		sort.Strings(completerWords)
	}
}

func removeWord(word string) {
	word = strings.ToLower(word)
	if words, lo, hi := WordMatch(word); words != nil {
		for ; lo <= hi; lo++ {
			if words[lo] == word {
				break
			}
		}
		if hi < lo {
			// Not in words.
			return
		}
		if lo < len(completerWords)-1 {
			copy(completerWords[lo:], completerWords[lo+1:])
		}
		completerWords = completerWords[:len(completerWords)-1]
	}
}
