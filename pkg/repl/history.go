// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
)

const (
	forwardDir  = 1
	backwardDir = -1
)

// History keeps a history of forms. Previously added Forms are stored in
// memory and also to a file.
type History struct {
	forms     []Form // first on form is oldest
	filename  string
	limit     int
	max       int    // limit * 1.1
	cur       int    // current position
	pattern   []rune // search pattern
	searchDir int    // forward, backward or 0 for not searching
}

// Load forms from a file and use that file for updates to history.
func (h *History) Load(filename string) {
	h.filename = filename
	h.forms = nil
	f, err := os.Open(filename)
	if err != nil {
		return
	}
	r := NewLineReader(f, 4096)
	var line []byte
	for {
		if line, err = r.ReadLine(); err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			panic(err)
		}
		line = bytes.TrimSpace(line)
		if 0 < len(line) {
			var form Form
			for _, sub := range bytes.Split(line, []byte{'\t'}) {
				form = append(form, []rune(string(sub)))
			}
			h.forms = append(h.forms, form)
		}
	}
}

// SetLimit of the history. The limit is the target maximum number of forms
// saved.
func (h *History) SetLimit(limit int) {
	h.limit = limit
	h.max = h.limit + h.limit/10
}

// Add adds a form to history.
func (h *History) Add(form Form) {
	if h.limit <= 0 || form.Empty() {
		return
	}
	if 0 < len(h.forms) && form.Equal(h.forms[len(h.forms)-1]) {
		return
	}
	h.forms = append(h.forms, form.Dup())
	if h.max <= len(h.forms) {
		h.forms = h.forms[len(h.forms)-h.limit:]
		tmp := fmt.Sprintf("%s.tmp", h.filename)
		f, err := os.OpenFile(tmp, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
		if err != nil {
			panic(err)
		}
		defer func() { _ = f.Close() }()
		for _, frm := range h.forms {
			// Write each line separately to avoid excessive memory use if the
			// history is long.
			if _, err = f.Write(frm.Append(nil)); err != nil {
				panic(err)
			}
		}
		_ = f.Close()
		if err := os.Rename(tmp, h.filename); err != nil {
			panic(err)
		}
	} else {
		f, err := os.OpenFile(h.filename, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
		defer func() { _ = f.Close() }()
		if err == nil {
			_, err = f.Write(form.Append(nil))
		}
		if err != nil {
			panic(err)
		}
	}
}

// Nth form in history numbered from the most recent added form.
func (h *History) Nth(n int) (form Form) {
	n = len(h.forms) - n - 1
	if 0 <= n && n < len(h.forms) {
		form = h.forms[n]
	}
	return
}

// Cursor returns the current cursor position.
func (h *History) Cursor() int {
	return len(h.forms) - h.cur - 1
}

// SetCursor sets the current cursor position adjusted to from zero to one
// less than the length of the history.
func (h *History) SetCursor(pos int) {
	if pos < 0 {
		pos = 0
	} else if len(h.forms) <= pos {
		pos = len(h.forms) - 1
	}
	h.cur = len(h.forms) - pos - 1
}

// Get the form at the cursor in history.
func (h *History) Get() (form Form) {
	if 0 <= h.cur && h.cur < len(h.forms) {
		form = h.forms[h.cur]
	}
	return
}

// Size of the current history.
func (h *History) Size() int {
	return len(h.forms)
}

// SearchBack for a match to the provided target string starting with the
// cursor position.
func (h *History) SearchBack(target string) Form {
	start := h.cur
	for ; 0 <= start; start-- {
		if form := h.forms[start]; form.Contains(target) {
			return form
		}
	}
	return nil
}

// SearchForward for a match to the provided target string starting with the
// cursor at the start position.
func (h *History) SearchForward(target string) (form Form) {
	start := h.cur
	for ; start < len(h.forms); start++ {
		if form := h.forms[start]; form.Contains(target) {
			return form
		}
	}
	return nil
}
