// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
)

// History keeps a history of forms. Previously added Forms are stored in
// memory and also to a file.
type History struct {
	Stash
	limit int
	max   int // limit * 1.1
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
			if _, err = f.Write(frm.TabAppend(nil)); err != nil {
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
			_, err = f.Write(form.TabAppend(nil))
		}
		if err != nil {
			panic(err)
		}
	}
}
