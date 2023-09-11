// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net_test

import (
	"strings"
)

type bodyReader struct {
	sr *strings.Reader
}

func newBodyReader(s string) *bodyReader {
	return &bodyReader{sr: strings.NewReader(s)}
}

func (r *bodyReader) Read(b []byte) (n int, err error) {
	return r.sr.Read(b)
}

func (r *bodyReader) Close() (err error) {
	return nil
}
