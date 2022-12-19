// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"errors"
	"io"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/pkg/repl"
	_ "github.com/ohler55/slip/pkg/repl"
)

func TestLineReaderReadLine(t *testing.T) {
	r := strings.NewReader(`This is a line longer than the buffer.
This is not.
The end.`)
	lr := repl.NewLineReader(r, 10)

	line, err := lr.ReadLine()
	tt.Nil(t, err)
	tt.Equal(t, "This is a line longer than the buffer.", string(line))

	line, err = lr.ReadLine()
	tt.Nil(t, err)
	tt.Equal(t, "This is not.", string(line))

	line, err = lr.ReadLine()
	tt.Equal(t, true, errors.Is(err, io.EOF))
	tt.Equal(t, "The end.", string(line))
}

func TestLineReaderRead(t *testing.T) {
	r := strings.NewReader(`This is a line longer than the buffer.
This is not.
The end.`)
	lr := repl.NewLineReader(r, 10)
	buf := make([]byte, 10)
	cnt, err := lr.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 10, cnt)
	tt.Equal(t, "This is a ", string(buf))
}
