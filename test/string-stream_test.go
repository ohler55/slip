// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"io"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestStringStreamObject(t *testing.T) {
	stream := slip.NewStringStream([]byte("hello"))
	(&sliptest.Object{
		Target:    stream,
		String:    "#<STRING-STREAM>",
		Simple:    "#<STRING-STREAM>",
		Hierarchy: "string-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: stream, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			(&slip.StringStream{}).StreamType,
		},
		Eval: stream,
	}).Test(t)
	data, err := io.ReadAll(stream)
	tt.Nil(t, err)
	tt.Equal(t, "hello", string(data))

	tt.Equal(t, "hello", stream.Content())
}

func TestStringStreamRead(t *testing.T) {
	stream := slip.NewStringStream([]byte("a𝄢"))
	buf := make([]byte, 10)
	cnt, err := stream.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 5, cnt)
	tt.Equal(t, "a𝄢", string(buf[:cnt]))
}

func TestStringStreamReadAt(t *testing.T) {
	stream := slip.NewStringStream([]byte("hello"))
	buf := make([]byte, 10)
	cnt, err := stream.ReadAt(buf, 3)
	tt.Nil(t, err)
	tt.Equal(t, 2, cnt)
	tt.Equal(t, "lo", string(buf[:cnt]))

	cnt, err = stream.ReadAt(buf, -1)
	tt.Nil(t, err)
	tt.Equal(t, 5, cnt)
	tt.Equal(t, "hello", string(buf[:cnt]))

	_, err = stream.ReadAt(buf, 6)
	tt.NotNil(t, err) // EOF
}

func TestStringStreamReadRune(t *testing.T) {
	stream := slip.NewStringStream([]byte("a𝄢c"))
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
}

func TestStringStreamOpenClose(t *testing.T) {
	stream := slip.NewStringStream([]byte("a𝄢c"))
	_ = stream.Close()
	buf := make([]byte, 4)
	_, err := stream.Read(buf)
	tt.NotNil(t, err)
	_, _, err = stream.ReadRune()
	tt.NotNil(t, err)
	_, err = stream.ReadAt(buf, 2)
	tt.NotNil(t, err)

	_, err = stream.Write(buf)
	tt.NotNil(t, err)
	_, err = stream.WriteAt(buf, 2)
	tt.NotNil(t, err)

	_, err = stream.Seek(1, 2)
	tt.NotNil(t, err)

	b := stream.LastByte()
	tt.Equal(t, 0, b)
}

func TestStringStreamWrite(t *testing.T) {
	var stream slip.StringStream

	cnt, err := stream.Write([]byte("hello"))
	tt.Nil(t, err)
	tt.Equal(t, 5, cnt)
	tt.Equal(t, "hello", stream.Content())

	var n int64
	n, err = stream.Seek(2, 0)
	tt.Nil(t, err)
	tt.Equal(t, 2, n)

	cnt, err = stream.Write([]byte("mming"))
	tt.Nil(t, err)
	tt.Equal(t, 5, cnt)
	tt.Equal(t, "hemming", stream.Content())
}

func TestStringStreamWriteAt(t *testing.T) {
	var stream slip.StringStream

	cnt, err := stream.WriteAt([]byte("hello"), 0)
	tt.Nil(t, err)
	tt.Equal(t, 5, cnt)
	tt.Equal(t, "hello", stream.Content())

	cnt, err = stream.WriteAt([]byte("mming"), 2)
	tt.Nil(t, err)
	tt.Equal(t, 5, cnt)
	tt.Equal(t, "hemming", stream.Content())

	cnt, err = stream.WriteAt([]byte("lp"), 2)
	tt.Nil(t, err)
	tt.Equal(t, 2, cnt)
	tt.Equal(t, "helping", stream.Content())
}

func TestStringStreamSeek(t *testing.T) {
	var stream slip.StringStream

	cnt, err := stream.Write([]byte("abcd"))
	tt.Nil(t, err)
	tt.Equal(t, 4, cnt)

	var n int64
	n, err = stream.Seek(2, 0)
	tt.Nil(t, err)
	tt.Equal(t, 2, n)

	buf := []byte{0}
	cnt, err = stream.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 1, cnt)
	tt.Equal(t, 'c', buf[0])

	n, err = stream.Seek(-2, 1)
	tt.Nil(t, err)
	tt.Equal(t, 1, n)
	cnt, err = stream.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 1, cnt)
	tt.Equal(t, 'b', buf[0])

	n, err = stream.Seek(-1, 2)
	tt.Nil(t, err)
	tt.Equal(t, 3, n)
	cnt, err = stream.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 1, cnt)
	tt.Equal(t, 'd', buf[0])

	_, err = stream.Seek(5, 0)
	tt.NotNil(t, err)
}

func TestStringStreamLastByte(t *testing.T) {
	var stream slip.StringStream

	cnt, err := stream.Write([]byte("hello"))
	tt.Nil(t, err)
	tt.Equal(t, 5, cnt)
	tt.Equal(t, 'o', stream.LastByte())
}
