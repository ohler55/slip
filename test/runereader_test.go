// Copyright (c) 2026, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestRuneReaderNilReader(t *testing.T) {
	var rr slip.RuneReader
	buf := make([]byte, 4)

	tt.Panic(t, func() { _, _ = rr.Read(buf) })
	tt.Panic(t, func() { _, _, _ = rr.ReadRune() })
	tt.Panic(t, func() { _, _ = rr.ReadByte() })
}
