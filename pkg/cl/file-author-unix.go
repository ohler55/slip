//go:build !windows

package cl

import (
	"os/user"
	"strconv"
	"syscall"

	"github.com/ohler55/slip"
)

func getFileOwner(s *slip.Scope, path string) string {
	var (
		stat syscall.Stat_t
		err  error
	)
	if err = syscall.Stat(path, &stat); err == nil {
		var u *user.User
		if u, err = user.LookupId(strconv.Itoa(int(stat.Uid))); err == nil {
			return u.Username
		}
	}
	panic(slip.FileErrorNew(s, 0, slip.String(path), "file-author failed. %s", err))
}
