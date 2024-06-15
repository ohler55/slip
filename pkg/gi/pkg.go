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

	slip.DefConstant(slip.Symbol("sighup"), slip.Fixnum(1), "terminate process, terminal line hangup")
	slip.DefConstant(slip.Symbol("sigint"), slip.Fixnum(2), "terminate process, interrupt program")
	slip.DefConstant(slip.Symbol("sigquit"), slip.Fixnum(3), "create core image, quit program")
	slip.DefConstant(slip.Symbol("sigill"), slip.Fixnum(4), "create core image, illegal instruction")
	slip.DefConstant(slip.Symbol("sigtrap"), slip.Fixnum(5), "create core image, trace trap")
	slip.DefConstant(slip.Symbol("sigabrt"), slip.Fixnum(6), "create core image, abort program (formerly SIGIOT)")
	slip.DefConstant(slip.Symbol("sigemt"), slip.Fixnum(7), "create core image, emulate instruction executed")
	slip.DefConstant(slip.Symbol("sigfpe"), slip.Fixnum(8), "create core image, floating-point exception")
	slip.DefConstant(slip.Symbol("sigkill"), slip.Fixnum(9), "terminate process, kill program")
	slip.DefConstant(slip.Symbol("sigbus"), slip.Fixnum(10), "create core image, bus error")
	slip.DefConstant(slip.Symbol("sigsegv"), slip.Fixnum(11), "create core image, segmentation violation")
	slip.DefConstant(slip.Symbol("sigsys"), slip.Fixnum(12), "create core image, non-existent system call invoked")
	slip.DefConstant(slip.Symbol("sigpipe"), slip.Fixnum(13), "terminate process, write on a pipe with no reader")
	slip.DefConstant(slip.Symbol("sigalrm"), slip.Fixnum(14), "terminate process, real-time timer expired")
	slip.DefConstant(slip.Symbol("sigterm"), slip.Fixnum(15), "terminate process, software termination signal")
	slip.DefConstant(slip.Symbol("sigurg"), slip.Fixnum(16), "discard signal, urgent condition present on socket")
	slip.DefConstant(slip.Symbol("sigstop"), slip.Fixnum(17), "stop process, stop (cannot be caught or ignored)")
	slip.DefConstant(slip.Symbol("sigtstp"), slip.Fixnum(18), "stop process, stop signal generated from keyboard")
	slip.DefConstant(slip.Symbol("sigcont"), slip.Fixnum(19), "discard signal, continue after stop")
	slip.DefConstant(slip.Symbol("sigchld"), slip.Fixnum(20), "discard signal, child status has changed")
	slip.DefConstant(slip.Symbol("sigttin"), slip.Fixnum(21),
		"stop process, background read attempted from control terminal")
	slip.DefConstant(slip.Symbol("sigttou"), slip.Fixnum(22),
		"stop process, background write attempted to control terminal")
	slip.DefConstant(slip.Symbol("sigio"), slip.Fixnum(23),
		"discard signal, I/O is possible on a descriptor (see fcntl(2))")
	slip.DefConstant(slip.Symbol("sigxcpu"), slip.Fixnum(24),
		"terminate process, cpu time limit exceeded (see setrlimit(2))")
	slip.DefConstant(slip.Symbol("sigxfsz"), slip.Fixnum(25),
		"terminate process, file size limit exceeded (see setrlimit(2))")
	slip.DefConstant(slip.Symbol("sigvtalrm"), slip.Fixnum(26),
		"terminate process, virtual time alarm (see setitimer(2))")
	slip.DefConstant(slip.Symbol("sigprof"), slip.Fixnum(27),
		"terminate process, profiling timer alarm (see setitimer(2))")
	slip.DefConstant(slip.Symbol("sigwinch"), slip.Fixnum(28), "discard signal, Window size change")
	slip.DefConstant(slip.Symbol("siginfo"), slip.Fixnum(29), "discard signal, status request from keyboard")
}
