//go:build !windows

package gi

import "syscall"

func sendSignal(pid int, sig int) {
	if err := syscall.Kill(pid, syscall.Signal(sig)); err != nil {
		panic(err)
	}
}