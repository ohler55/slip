// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch_test

import "sync"

type safeWriter struct {
	buf []byte
	mu  sync.Mutex
}

func (w *safeWriter) Write(p []byte) (n int, err error) {
	w.mu.Lock()
	w.buf = append(w.buf, p...)
	w.mu.Unlock()
	return len(p), nil
}

func (w *safeWriter) Len() (n int) {
	w.mu.Lock()
	n = len(w.buf)
	w.mu.Unlock()
	return
}

func (w *safeWriter) String() (s string) {
	w.mu.Lock()
	s = string(w.buf)
	w.mu.Unlock()
	return
}

func (w *safeWriter) Reset() {
	w.mu.Lock()
	w.buf = w.buf[:0]
	w.mu.Unlock()
}
