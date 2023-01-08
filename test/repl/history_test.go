// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/pkg/repl"
)

func TestHistoryLoad(t *testing.T) {
	filename := "config/history"
	err := os.WriteFile(filename, []byte("one\ntwo\nthree\t3\tthird\nfour\n"), 0666)
	tt.Nil(t, err)

	var h repl.History
	h.Load(filename)
	tt.Equal(t, "four\n", h.Nth(0).String())
	tt.Equal(t, "three\n3\nthird\n", h.Nth(1).String())
	tt.Equal(t, "two\n", h.Nth(2).String())
	tt.Equal(t, "one\n", h.Nth(3).String())
	tt.Equal(t, "", h.Nth(4).String())
}

func TestHistoryLoadNoFile(t *testing.T) {
	var h repl.History
	// Should not panic.
	h.Load("not-a-file")
}

func TestHistoryUseCase(t *testing.T) {
	filename := "config/history"
	err := os.WriteFile(filename, []byte("\n"), 0666)
	tt.Nil(t, err)

	var h repl.History
	h.SetLimit(10)
	h.Load(filename)

	h.Add(repl.Form{[]rune("one")})
	h.Add(repl.Form{[]rune("two")})
	h.Add(repl.Form{[]rune("three"), []rune("3"), []rune("third")})
	h.Add(repl.Form{[]rune("four")})

	tt.Equal(t, 4, h.Size())

	h.SetCursor(100)
	tt.Equal(t, 3, h.Cursor())
	tt.Equal(t, "one\n", h.Get().String())

	h.SetCursor(h.Cursor() - 1)
	tt.Equal(t, "two\n", h.Get().String())

	h.SetCursor(-1)
	tt.Equal(t, 0, h.Cursor())
	tt.Equal(t, "four\n", h.Get().String())

	h.SetCursor(1)
	f := h.SearchBack("wo")
	tt.Equal(t, "two\n", f.String())

	h.SetCursor(3) // before "two" was added
	f = h.SearchBack("wo")
	tt.Equal(t, "", f.String())

	h.SetCursor(10)
	f = h.SearchForward("wo")
	tt.Equal(t, "two\n", f.String())

	h.SetCursor(1) // after "two" was added
	f = h.SearchForward("wo")
	tt.Equal(t, "", f.String())
}

func TestHistoryLimit(t *testing.T) {
	filename := "config/history"
	err := os.WriteFile(filename, []byte("\n"), 0666)
	tt.Nil(t, err)

	var h repl.History
	h.SetLimit(0)
	h.Add(repl.Form{[]rune("not added")})
	tt.Equal(t, 0, h.Size())

	h.SetLimit(10)
	h.Load(filename)

	for i := 0; i < 10; i++ {
		h.Add(repl.Form{[]rune(fmt.Sprintf("entry %d", i))})
	}
	tt.Equal(t, 10, h.Size())

	// Add another to put it at the max of 1.1 * limit.
	h.Add(repl.Form{[]rune("entry 10")})
	tt.Equal(t, 10, h.Size())

	var h2 repl.History
	h2.SetLimit(20)
	h2.Load(filename)
	tt.Equal(t, 10, h2.Size())
}
