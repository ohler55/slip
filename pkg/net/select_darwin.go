// Copyright (c) 2024, Peter Ohler, All rights reserved.

//go:build darwin

package net

import (
	"syscall"
	"time"
)

type FdSet syscall.FdSet

func Select(r, w, e *FdSet, timeout time.Duration) error {
	var high int

	tv := syscall.NsecToTimeval(int64(timeout))
	for _, fs := range []*FdSet{r, w, e} {
		if fs != nil {
			hi := fs.Highest()
			if high < hi {
				high = hi
			}
		}
	}
	return syscall.Select(high+1, (*syscall.FdSet)(r), (*syscall.FdSet)(w), (*syscall.FdSet)(e), &tv)
}

func (fs *FdSet) Set(fd int) {
	fs.Bits[fd/32] |= 0x01 << (fd % 32)
}

func (fs *FdSet) IsSet(fd int) bool {
	return (fs.Bits[fd/32] & (0x01 << (fd % 32))) != 0
}

func (fs *FdSet) Highest() (high int) {
	var n int
	for i, b := range fs.Bits {
		if b != 0 {
			for j := 31; 0 <= j; j-- {
				if (b & (0x01 << j)) != 0 {
					n = i*32 + j
					if high < n {
						high = n
					}
					break
				}
			}
		}
	}
	return
}
