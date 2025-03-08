// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"

	"github.com/ohler55/slip"
)

const (
	forwardDir  = 1
	backwardDir = -1
)

// Stash is used for history and general use stashes which are a collection of
// LISP expressions.
type Stash struct {
	forms     []Form // first on form is oldest
	filename  string
	cur       int    // current position
	pattern   []rune // search pattern
	searchDir int    // forward, backward or 0 for not searching
}

// LoadExpanded forms from a file and use that file for updates to history.
func (s *Stash) LoadExpanded(filename string) {
	s.filename = filename
	s.forms = nil
	f, err := os.Open(filename)
	if err != nil {
		return
	}
	r := NewLineReader(f, 4096)
	var (
		line []byte
		buf  []byte
		form Form
	)
	for {
		if line, err = r.ReadLine(); err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			panic(err)
		}
		if 0 < len(line) {
			if bytes.ContainsRune(line, '\t') {
				for _, sub := range bytes.Split(line, []byte{'\t'}) {
					buf = append(buf, sub...)
					buf = append(buf, '\n')
					form = append(form, []rune(string(sub)))
				}
			} else {
				buf = append(buf, line...)
				buf = append(buf, '\n')
				if 0 < len(form) || 0 < len(line) {
					form = append(form, []rune(string(line)))
				}
			}
			if fullForm(buf) {
				s.forms = append(s.forms, form)
				buf = buf[:0]
				form = nil
			}
		}
	}
}

// Add adds a form to the stash.
func (s *Stash) Add(form Form) {
	if form.Empty() {
		return
	}
	if 0 < len(s.forms) && form.Equal(s.forms[len(s.forms)-1]) {
		return
	}
	s.forms = append(s.forms, form.Dup())
	if 0 < len(s.filename) {
		f, err := os.OpenFile(s.filename, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
		if err == nil {
			defer func() { _ = f.Close() }()
			_, err = f.Write(append(form.Append(nil), '\n'))
		}
		if err != nil {
			panic(err)
		}
	}
}

// Append the stash forms in order as expanded LISP code.
func (s *Stash) Append(b []byte, annotate, tight, raw bool, start, end int) []byte {
	if start < 0 {
		start = 0
	} else if len(s.forms) <= start {
		return b
	}
	if end < 0 || len(s.forms) < end {
		end = len(s.forms)
	}
	for ; start < end; start++ {
		f := s.forms[start]
		if raw {
			b = f.TabAppend(b)
		} else {
			if !tight {
				b = append(b, '\n')
			}
			if annotate {
				b = fmt.Appendf(b, ";; %d\n", start+1)
			}
			if 0 < len(f) {
				b = f.Append(b)
			}
		}
	}
	return b
}

// Nth form in stash numbered from the most recent added form.
func (s *Stash) Nth(n int) (form Form) {
	n = len(s.forms) - n - 1
	if 0 <= n && n < len(s.forms) {
		form = s.forms[n]
	}
	return
}

// Cursor returns the current cursor position.
func (s *Stash) Cursor() int {
	return len(s.forms) - s.cur - 1
}

// SetCursor sets the current cursor position adjusted to from zero to one
// less than the length of the history.
func (s *Stash) SetCursor(pos int) {
	if pos < 0 {
		pos = 0
	} else if len(s.forms) <= pos {
		pos = len(s.forms) - 1
	}
	s.cur = len(s.forms) - pos - 1
}

// Get the form at the cursor in history.
func (s *Stash) Get() (form Form) {
	if 0 <= s.cur && s.cur < len(s.forms) {
		form = s.forms[s.cur]
	}
	return
}

// Size of the current history.
func (s *Stash) Size() int {
	return len(s.forms)
}

// Clear the stash entries in the range specified..
func (s *Stash) Clear(start, end int) {
	s.clear(start, end)
	f, err := os.OpenFile(s.filename, os.O_TRUNC|os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		panic(err)
	}
	defer func() { _ = f.Close() }()
	for _, frm := range s.forms {
		if _, err = f.Write(frm.TabAppend(nil)); err != nil {
			panic(err)
		}
	}
}

func (s *Stash) clear(start, end int) {
	if 0 < len(s.forms) && start < len(s.forms) {
		if start < 0 {
			start = 0
		}
		if end < 0 || len(s.forms) <= end {
			end = len(s.forms) - 1
		}
		if start <= end {
			newEnd := len(s.forms) - (end - start) - 1
			copy(s.forms[:start], s.forms[end:])
			// Make sure references are removed so GC can collect them.
			for i := end + 1; i < len(s.forms); i++ {
				s.forms[i] = nil
			}
			s.forms = s.forms[:newEnd]
		}
	}
}

// SearchBack for a match to the provided target string starting with the
// cursor position.
func (s *Stash) SearchBack(target string) Form {
	start := s.cur
	for ; 0 <= start; start-- {
		if form := s.forms[start]; form.Contains(target) {
			return form
		}
	}
	return nil
}

// SearchForward for a match to the provided target string starting with the
// cursor at the start position.
func (s *Stash) SearchForward(target string) (form Form) {
	start := s.cur
	for ; start < len(s.forms); start++ {
		if form := s.forms[start]; form.Contains(target) {
			return form
		}
	}
	return nil
}

func fullForm(buf []byte) (full bool) {
	defer func() {
		if rec := recover(); rec != nil {
			if _, ok := rec.(*slip.PartialPanic); ok {
				full = false
			} else {
				panic(rec)
			}
		}
	}()
	_ = slip.Read(buf, &scope)

	return true
}
