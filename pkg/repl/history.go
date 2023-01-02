// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
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
	f, err := os.Open(filename)
	if err != nil {
		return
	}
	h.filename = filename
	r := NewLineReader(f, 4096)
	var line []byte
	for {
		if line, err = r.ReadLine(); err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			panic(err)
		}
		var form Form
		for _, sub := range bytes.Split(line, []byte{'\t'}) {
			form = append(form, []rune(string(sub)))
		}
		h.forms = append(h.forms, form)
	}
}

// SetLimit of the history. The limit is the target maximum number of forms
// saved.
func (h *History) SetLimit(limit int) {
	h.limit = limit
	h.max = h.limit + h.limit/10
}

// AddForm adds a form to history.
func (h *History) AddForm(form Form, ed *editor) {
	if h.limit <= 0 {
		return
	}
	h.forms = append(h.forms, formDup(form))
	if h.max <= len(h.forms) {
		h.forms = h.forms[len(h.forms)-h.limit:]
		tmp := fmt.Sprintf("%s.tmp", h.filename)
		f, err := os.OpenFile(tmp, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
		if err != nil {
			panic(err)
		}
		defer func() { _ = f.Close() }()
		var entry []byte
		// Write each line separately to avoid excessive memory use if the
		// history is long.
		for _, frm := range ed.lines {
			for _, line := range frm {
				entry = append(entry, string(line)...)
				entry = append(entry, '\t')
			}
			entry[len(entry)-1] = '\n'
			// TBD if too long don't write
			if _, err = f.Write(entry); err != nil {
				panic(err)
			}
		}
		_ = f.Close()
		if err := os.Rename(tmp, h.filename); err != nil {
			panic(err)
		}
	} else {
		f, err := os.OpenFile(h.filename, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
		if err != nil {
			panic(err)
		}
		defer func() { _ = f.Close() }()
		var entry []byte
		for _, line := range form {
			entry = append(entry, string(line)...)
			entry = append(entry, '\t')
		}
		entry[len(entry)-1] = '\n'
		// TBD if too long don't write
		if _, err = f.Write(entry); err != nil {
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
// cursor at the start position.
func (h *History) SearchBack(start int, target string) Form {
	for ; 0 <= start; start-- {
		form := h.forms[start]
		for _, line := range form {
			if strings.Contains(string(line), target) {
				return form
			}
		}
	}
	return nil
}

// SearchForward for a match to the provided target string starting with the
// cursor at the start position.
func (h *History) SearchForward(start int, target string) (form Form) {
	for ; start < len(h.forms); start++ {
		form := h.forms[start]
		for _, line := range form {
			if strings.Contains(string(line), target) {
				return form
			}
		}
	}
	return nil
}
