// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"io"
	"strings"
)

type bodyWrap struct {
	r io.Reader
}

func bodyWrapString(s string) *bodyWrap {
	return &bodyWrap{r: strings.NewReader(s)}
}

func bodyWrapReader(r io.Reader) *bodyWrap {
	return &bodyWrap{r: r}
}

func (bw *bodyWrap) Read(b []byte) (n int, err error) {
	return bw.r.Read(b)
}

func (bw *bodyWrap) Close() (err error) {
	if c, ok := bw.r.(io.Closer); ok {
		return c.Close()
	}
	return nil
}
