// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"io"
	"io/ioutil"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestInputStreamObject(t *testing.T) {
	stream := slip.InputStream{Reader: strings.NewReader("abc")}
	(&sliptest.Object{
		Target:    &stream,
		String:    "#<INPUT-STREAM>",
		Simple:    "#<INPUT-STREAM>",
		Hierarchy: "input-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: &stream, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			(&slip.InputStream{}).StreamType,
		},
		Eval: &stream,
	}).Test(t)
	data, err := ioutil.ReadAll(&stream)
	tt.Nil(t, err)
	tt.Equal(t, "abc", string(data))
}

func TestInputStreamReadRune(t *testing.T) {
	stream := slip.InputStream{Reader: strings.NewReader("a𝄢c")}
	r, size, err := stream.ReadRune()
	tt.Equal(t, 'a', r)
	tt.Equal(t, 1, size)
	tt.Nil(t, err)

	r, size, err = stream.ReadRune()
	tt.Equal(t, '𝄢', r)
	tt.Equal(t, 4, size)
	tt.Nil(t, err)

	r, size, err = stream.ReadRune()
	tt.Equal(t, 'c', r)
	tt.Equal(t, 1, size)
	tt.Nil(t, err)

	_, _, err = stream.ReadRune()
	tt.NotNil(t, err)
}

type justReader struct {
	buf []byte
	pos int
}

func (r *justReader) Read(b []byte) (int, error) {
	if len(r.buf)-r.pos < len(b) {
		return 0, io.EOF
	}
	copy(b, r.buf[r.pos:])
	r.pos += len(b)
	return len(b), nil
}

func TestInputStreamReadRuneHard(t *testing.T) {
	stream := slip.InputStream{Reader: &justReader{buf: []byte("a𝄢c")}}
	r, size, err := stream.ReadRune()
	tt.Equal(t, 'a', r)
	tt.Equal(t, 1, size)
	tt.Nil(t, err)

	r, size, err = stream.ReadRune()
	tt.Equal(t, '𝄢', r)
	tt.Equal(t, 4, size)
	tt.Nil(t, err)

	r, size, err = stream.ReadRune()
	tt.Equal(t, 'c', r)
	tt.Equal(t, 1, size)
	tt.Nil(t, err)

	_, _, err = stream.ReadRune()
	tt.NotNil(t, err)
	// fmt.Printf("*** rune: %s  size: %d  err: %v\n", string([]rune{r}), size, err)
}
