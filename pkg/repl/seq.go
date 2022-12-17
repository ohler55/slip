// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

type seq struct {
	cnt int
	buf []byte
}

func (s *seq) same(s2 *seq) bool {
	if s.cnt == s2.cnt {
		for i := s.cnt - 1; 0 <= i; i-- {
			if s.buf[i] != s2.buf[i] {
				return false
			}
		}
		return true
	}
	return false
}

func (s *seq) set(s2 *seq) {
	s.cnt = s2.cnt
	copy(s.buf, s2.buf)
}

func (s *seq) dup() *seq {
	s2 := seq{cnt: s.cnt, buf: make([]byte, 8)}
	copy(s2.buf, s.buf)
	return &s2
}
