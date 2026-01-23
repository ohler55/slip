//go:build !windows

package cl

import (
	"os/user"
	"strconv"
	"syscall"
)

func getFileOwner(path string) string {
	var stat syscall.Stat_t
	if err := syscall.Stat(path, &stat); err == nil {
		if u, err := user.LookupId(strconv.Itoa(int(stat.Uid))); err == nil {
			return u.Username
		}
	}
	return ""
}