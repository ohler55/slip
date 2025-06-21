// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the Gi package.
	Pkg = slip.Package{
		Name:      "gi",
		Nicknames: []string{"go-integration", "golang-integration"},
		Doc:       "Home of symbols defined for the Go Integration functions, variables, and constants.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	defLogger()
	defSystem()
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*gi*":             {Val: &Pkg, Doc: Pkg.Doc, Export: true},
			"*go-integration*": {Val: &Pkg, Doc: Pkg.Doc, Export: true},
		},
		&Env{},
	)
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)

	slip.DefConstant(&Pkg, "sighup", slip.Fixnum(1), "terminate process, terminal line hangup")
	slip.DefConstant(&Pkg, "sigint", slip.Fixnum(2), "terminate process, interrupt program")
	slip.DefConstant(&Pkg, "sigquit", slip.Fixnum(3), "create core image, quit program")
	slip.DefConstant(&Pkg, "sigill", slip.Fixnum(4), "create core image, illegal instruction")
	slip.DefConstant(&Pkg, "sigtrap", slip.Fixnum(5), "create core image, trace trap")
	slip.DefConstant(&Pkg, "sigabrt", slip.Fixnum(6), "create core image, abort program (formerly SIGIOT)")
	slip.DefConstant(&Pkg, "sigemt", slip.Fixnum(7), "create core image, emulate instruction executed")
	slip.DefConstant(&Pkg, "sigfpe", slip.Fixnum(8), "create core image, floating-point exception")
	slip.DefConstant(&Pkg, "sigkill", slip.Fixnum(9), "terminate process, kill program")
	slip.DefConstant(&Pkg, "sigbus", slip.Fixnum(10), "create core image, bus error")
	slip.DefConstant(&Pkg, "sigsegv", slip.Fixnum(11), "create core image, segmentation violation")
	slip.DefConstant(&Pkg, "sigsys", slip.Fixnum(12), "create core image, non-existent system call invoked")
	slip.DefConstant(&Pkg, "sigpipe", slip.Fixnum(13), "terminate process, write on a pipe with no reader")
	slip.DefConstant(&Pkg, "sigalrm", slip.Fixnum(14), "terminate process, real-time timer expired")
	slip.DefConstant(&Pkg, "sigterm", slip.Fixnum(15), "terminate process, software termination signal")
	slip.DefConstant(&Pkg, "sigurg", slip.Fixnum(16), "discard signal, urgent condition present on socket")
	slip.DefConstant(&Pkg, "sigstop", slip.Fixnum(17), "stop process, stop (cannot be caught or ignored)")
	slip.DefConstant(&Pkg, "sigtstp", slip.Fixnum(18), "stop process, stop signal generated from keyboard")
	slip.DefConstant(&Pkg, "sigcont", slip.Fixnum(19), "discard signal, continue after stop")
	slip.DefConstant(&Pkg, "sigchld", slip.Fixnum(20), "discard signal, child status has changed")
	slip.DefConstant(&Pkg, "sigttin", slip.Fixnum(21),
		"stop process, background read attempted from control terminal")
	slip.DefConstant(&Pkg, "sigttou", slip.Fixnum(22),
		"stop process, background write attempted to control terminal")
	slip.DefConstant(&Pkg, "sigio", slip.Fixnum(23),
		"discard signal, I/O is possible on a descriptor (see fcntl(2))")
	slip.DefConstant(&Pkg, "sigxcpu", slip.Fixnum(24),
		"terminate process, cpu time limit exceeded (see setrlimit(2))")
	slip.DefConstant(&Pkg, "sigxfsz", slip.Fixnum(25),
		"terminate process, file size limit exceeded (see setrlimit(2))")
	slip.DefConstant(&Pkg, "sigvtalrm", slip.Fixnum(26),
		"terminate process, virtual time alarm (see setitimer(2))")
	slip.DefConstant(&Pkg, "sigprof", slip.Fixnum(27),
		"terminate process, profiling timer alarm (see setitimer(2))")
	slip.DefConstant(&Pkg, "sigwinch", slip.Fixnum(28), "discard signal, Window size change")
	slip.DefConstant(&Pkg, "siginfo", slip.Fixnum(29), "discard signal, status request from keyboard")
}

func seqStarEndArgs(args slip.List) (start, end int) {
	start = 0
	end = -1
	if v, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":start")); has {
		if num, ok := v.(slip.Fixnum); ok && 0 <= num {
			start = int(num)
		} else {
			slip.PanicType(":start", v, "fixnum")
		}
	}
	if v, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":end")); has && v != nil {
		if num, ok := v.(slip.Fixnum); ok && 0 <= num {
			end = int(num)
		} else {
			slip.PanicType(":start", v, "fixnum")
		}
	}
	var size int
	switch ta := args[0].(type) {
	case nil:
		// size remains as 0
	case slip.Octets:
		size = len(ta)
	case slip.String:
		size = len([]rune(ta))
	case slip.List:
		size = len(ta)
	case *slip.Vector:
		size = ta.Length()
	}
	if end < 0 {
		end = size
	}
	if size < start {
		slip.NewPanic("start, %d is greater than length of %d", start, size)
	}
	if size < end {
		slip.NewPanic("end, %d is greater than length of %d", end, size)
	}
	if end < start {
		slip.NewPanic("end, %d is less start %d", end, start)
	}
	return
}
