// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
)

const maxFormSize = 16384

type history struct {
	forms    [][][]rune // first on form is oldest
	filename string
	limit    int
	max      int // limit * 1.1
	cur      int // current position
	keys     []*seq
}

func (h *history) load() {
	f, err := os.Open(h.filename)
	if err != nil {
		return
	}
	r := bufio.NewReaderSize(f, maxFormSize)
	var line []byte
	for {
		if line, _, err = r.ReadLine(); err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			panic(err)
		}
		var form [][]rune
		for _, sub := range bytes.Split(line, []byte{'\t'}) {
			form = append(form, []rune(string(sub)))
		}
		h.forms = append(h.forms, form)
	}
}

func (h *history) setLimit(limit int) {
	h.limit = limit
	h.max = h.limit + h.limit/10
}

func (h *history) addForm(form [][]rune, ed *editor) {
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

// reverse order
func (h *history) getNth(n int) (form [][]rune) {
	n = len(h.forms) - n - 1
	if 0 <= n && n < len(h.forms) {
		form = h.forms[n]
	}
	return
}

func (h *history) get() (form [][]rune) {
	if 0 <= h.cur && h.cur < len(h.forms) {
		form = h.forms[h.cur]
	}
	return
}

func (h *history) size() int {
	return len(h.forms)
}

func (h *history) searchBack(start int, s string) [][]rune {
	for ; 0 <= start; start-- {
		form := h.forms[start]
		for _, line := range form {
			if strings.Contains(string(line), s) {
				return form
			}
		}
	}
	return nil
}

func (h *history) searchForward(start int, s string) (form [][]rune) {
	for ; start < len(h.forms); start++ {
		form := h.forms[start]
		for _, line := range form {
			if strings.Contains(string(line), s) {
				return form
			}
		}
	}
	return nil
}

func (h *history) addKey(k *seq) {
	for _, s := range h.keys {
		if s.same(k) {
			return
		}
	}
	h.keys = append(h.keys, k.dup())
}

func (h *history) hasKey(k *seq) bool {
	for _, s := range h.keys {
		if s.same(k) {
			return true
		}
	}
	return false
}
