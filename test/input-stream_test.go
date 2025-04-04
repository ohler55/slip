// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"io"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

type justReader struct {
	buf []byte
	pos int
}

func (r *justReader) Read(b []byte) (cnt int, err error) {
	if len(r.buf) <= r.pos {
		return 0, io.EOF
	}
	cnt = len(r.buf) - r.pos
	if len(b) < cnt {
		cnt = len(b)
	}
	copy(b, r.buf[r.pos:])
	r.pos += cnt
	return
}

func (r *justReader) Close() error {
	return nil
}

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
	data, err := io.ReadAll(&stream)
	tt.Nil(t, err)
	tt.Equal(t, "abc", string(data))
}

func TestInputStreamReadEasy(t *testing.T) {
	stream := slip.InputStream{Reader: strings.NewReader("a𝄢")}
	buf := make([]byte, 10)
	cnt, err := stream.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 5, cnt)
	tt.Equal(t, "a𝄢", string(buf[:cnt]))

	err = stream.UnreadRune()
	tt.Nil(t, err)

	cnt, err = stream.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 4, cnt)
	tt.Equal(t, "𝄢", string(buf[:cnt]))
}

func TestInputStreamReadHard(t *testing.T) {
	stream := slip.InputStream{Reader: &justReader{buf: []byte("a𝄢")}}
	buf := make([]byte, 10)
	cnt, err := stream.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 5, cnt)
	tt.Equal(t, "a𝄢", string(buf[:cnt]))

	err = stream.UnreadRune()
	tt.Nil(t, err)

	cnt, err = stream.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 4, cnt)
	tt.Equal(t, "𝄢", string(buf[:cnt]))

	err = stream.UnreadRune()
	tt.Nil(t, err)

	// too short for rune
	_, err = stream.Read(buf[:3])
	tt.NotNil(t, err)

	stream = slip.InputStream{Reader: &justReader{buf: []byte("abc")}}
	cnt, err = stream.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 3, cnt)
	tt.Equal(t, "abc", string(buf[:cnt]))

	err = stream.UnreadRune()
	tt.Nil(t, err)

	cnt, err = stream.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 1, cnt)
	tt.Equal(t, "c", string(buf[:cnt]))
}

func TestInputStreamReadRune(t *testing.T) {
	stream := slip.InputStream{Reader: strings.NewReader("a𝄢c")}
	r, size, err := stream.ReadRune()
	tt.Equal(t, 'a', r)
	tt.Equal(t, 1, size)
	tt.Nil(t, err)

	tt.Equal(t, true, stream.IsOpen())

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

	// EOF  also means closed.
	tt.Equal(t, false, stream.IsOpen())

	_ = stream.Close()
	tt.Equal(t, false, stream.IsOpen())
}

func TestInputStreamReadRuneHardOk(t *testing.T) {
	stream := slip.InputStream{Reader: &justReader{buf: []byte("a𝄢༒¥")}}
	r, size, err := stream.ReadRune()
	tt.Equal(t, 'a', r)
	tt.Equal(t, 1, size)
	tt.Nil(t, err)

	r, size, err = stream.ReadRune()
	tt.Equal(t, '𝄢', r)
	tt.Equal(t, 4, size)
	tt.Nil(t, err)

	r, size, err = stream.ReadRune()
	tt.Equal(t, '༒', r)
	tt.Equal(t, 3, size)
	tt.Nil(t, err)

	r, size, err = stream.ReadRune()
	tt.Equal(t, '¥', r)
	tt.Equal(t, 2, size)
	tt.Nil(t, err)

	err = stream.UnreadRune()
	tt.Nil(t, err)

	r, size, err = stream.ReadRune()
	tt.Equal(t, '¥', r)
	tt.Equal(t, 2, size)
	tt.Nil(t, err)

	_, _, err = stream.ReadRune()
	tt.NotNil(t, err)

	stream.Close()
}

func TestInputStreamReadRuneHardBad(t *testing.T) {
	stream := slip.InputStream{Reader: &justReader{buf: []byte("\x90")}}
	_, _, err := stream.ReadRune()
	tt.NotNil(t, err)
}

func TestInputStreamUnreadRune(t *testing.T) {
	stream := slip.InputStream{Reader: &justReader{buf: []byte("𝄢")}}
	r, size, err := stream.ReadRune()
	tt.Equal(t, '𝄢', r)
	tt.Equal(t, 4, size)
	tt.Nil(t, err)

	err = stream.UnreadRune()
	tt.Nil(t, err)

	err = stream.UnreadRune()
	tt.NotNil(t, err)

	r, size, err = stream.ReadRune()
	tt.Equal(t, '𝄢', r)
	tt.Equal(t, 4, size)
	tt.Nil(t, err)

	stream = slip.InputStream{Reader: strings.NewReader("𝄢")}
	r, size, err = stream.ReadRune()
	tt.Equal(t, '𝄢', r)
	tt.Equal(t, 4, size)
	tt.Nil(t, err)

	err = stream.UnreadRune()
	tt.Nil(t, err)

	r, size, err = stream.ReadRune()
	tt.Equal(t, '𝄢', r)
	tt.Equal(t, 4, size)
	tt.Nil(t, err)
}

func TestInputStreamReadByteEasy(t *testing.T) {
	stream := slip.InputStream{Reader: strings.NewReader("a𝄢")}
	b, err := stream.ReadByte()
	tt.Nil(t, err)
	tt.Equal(t, 'a', b)

	err = stream.UnreadRune()
	tt.Nil(t, err)

	b, err = stream.ReadByte()
	tt.Nil(t, err)
	tt.Equal(t, 'a', b)

	_, _, err = stream.ReadRune()
	tt.Nil(t, err)
	err = stream.UnreadRune()
	tt.Nil(t, err)

	_, err = stream.ReadByte()
	tt.NotNil(t, err)
}

func TestInputStreamReadByteHard(t *testing.T) {
	stream := slip.InputStream{Reader: &justReader{buf: []byte("a𝄢")}}
	b, err := stream.ReadByte()
	tt.Nil(t, err)
	tt.Equal(t, 'a', b)

	err = stream.UnreadRune()
	tt.Nil(t, err)

	b, err = stream.ReadByte()
	tt.Nil(t, err)
	tt.Equal(t, 'a', b)
}

func TestInputStreamPushRune(t *testing.T) {
	stream := slip.InputStream{Reader: strings.NewReader("abc")}

	stream.PushRune('x')

	r, _, err := stream.ReadRune()
	tt.Nil(t, err)
	tt.Equal(t, 'x', r)
}
