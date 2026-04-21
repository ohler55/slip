//go:build windows

package gi

import "fmt"

func sendSignal(pid int, sig int) {
	panic(fmt.Errorf("send-signal is not supported on Windows (pid=%d, signal=%d)", pid, sig))
}
