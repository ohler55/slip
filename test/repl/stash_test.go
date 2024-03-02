// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"os"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/pkg/repl"
)

func TestStashLoadExpanded(t *testing.T) {
	filename := "config/history"
	err := os.WriteFile(filename, []byte("one\n(+ 1\t   2\t   3)\n\n(* 2\n   four)\n"), 0666)
	tt.Nil(t, err)

	var s repl.Stash
	s.LoadExpanded(filename)
	tt.Equal(t, "(* 2\n   four)\n", s.Nth(0).String())
	tt.Equal(t, "(+ 1\n   2\n   3)\n", s.Nth(1).String())
	tt.Equal(t, "one\n", s.Nth(2).String())

	tt.Equal(t, `one
(+ 1	   2	   3)
(* 2	   four)
`, string(s.Append(nil, false, false, true, 0, -1)))

	tt.Equal(t, `one
(+ 1
   2
   3)
(* 2
   four)
`, string(s.Append(nil, false, true, false, 0, -1)))

	tt.Equal(t, `
one

(+ 1
   2
   3)

(* 2
   four)
`, string(s.Append(nil, false, false, false, 0, -1)))

	tt.Equal(t, `;; 1
one
;; 2
(+ 1
   2
   3)
;; 3
(* 2
   four)
`, string(s.Append(nil, true, true, false, 0, -1)))

	tt.Equal(t, `one
(+ 1	   2	   3)
(* 2	   four)
`, string(s.Append(nil, false, false, true, 0, -1)))
}

func TestStashLoadNoFile(t *testing.T) {
	var s repl.Stash
	// Should not panic.
	s.LoadExpanded("not-a-file")
}

func TestStashUseCase(t *testing.T) {
	filename := "config/history"
	err := os.WriteFile(filename, []byte("\n"), 0666)
	tt.Nil(t, err)

	var s repl.Stash
	s.LoadExpanded(filename)

	s.Add(repl.Form{[]rune("one")})
	s.Add(repl.Form{[]rune("two")})
	s.Add(repl.Form{[]rune("three"), []rune("3"), []rune("third")})
	s.Add(repl.Form{[]rune("four")})

	tt.Equal(t, 4, s.Size())

	s.SetCursor(100)
	tt.Equal(t, 3, s.Cursor())
	tt.Equal(t, "one\n", s.Get().String())

	s.SetCursor(s.Cursor() - 1)
	tt.Equal(t, "two\n", s.Get().String())

	s.SetCursor(-1)
	tt.Equal(t, 0, s.Cursor())
	tt.Equal(t, "four\n", s.Get().String())

	s.SetCursor(1)
	f := s.SearchBack("wo")
	tt.Equal(t, "two\n", f.String())

	s.SetCursor(3) // before "two" was added
	f = s.SearchBack("wo")
	tt.Equal(t, "", f.String())

	s.SetCursor(10)
	f = s.SearchForward("wo")
	tt.Equal(t, "two\n", f.String())

	s.SetCursor(1) // after "two" was added
	f = s.SearchForward("wo")
	tt.Equal(t, "", f.String())

	var content []byte
	content, err = os.ReadFile(filename)
	tt.Nil(t, err)
	tt.Equal(t, `
one

two

three
3
third

four

`, string(content))
}

func TestStashAdd(t *testing.T) {
	var s repl.Stash
	filename := "config/quux"
	_ = os.RemoveAll(filename)
	err := os.WriteFile(filename, []byte{}, 0666)
	tt.Nil(t, err)

	s.LoadExpanded(filename)
	s.Add(repl.NewForm([]byte("(+ 1 2)")))
	s.Add(repl.NewForm([]byte("(+ 1 2)")))
	s.Add(repl.NewForm([]byte{}))
	tt.Equal(t, 1, s.Size())

	err = os.Chmod(filename, 0x220)
	// _ = os.Remove(filename)
	// time.Sleep(time.Second)
	// err = os.Mkdir(filename, 0x666)
	fmt.Printf("*** err: %s\n", err)
	time.Sleep(time.Second * 10)
	// defer func() { _ = os.RemoveAll(filename) }()
	// TBD create dir?
	s.Add(repl.NewForm([]byte("(+ 1 2)")))

	// tt.Panic(t, func() { s.Add(repl.NewForm([]byte("(+ 1 2)"))) })

}
