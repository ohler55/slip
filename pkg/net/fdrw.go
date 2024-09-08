// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net

import "syscall"

type fdRW int

// Read p from the file descriptor.
func (fd fdRW) Read(p []byte) (n int, err error) {
	return syscall.Read(int(fd), p)
}

// Write p to the file descriptor.
func (fd fdRW) Write(p []byte) (n int, err error) {
	return syscall.Write(int(fd), p)
}
