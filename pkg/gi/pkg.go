// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
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
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*gi*":             {Val: &Pkg, Doc: Pkg.Doc, Const: true, Export: true},
			"*go-integration*": {Val: &Pkg, Doc: Pkg.Doc, Const: true, Export: true},
			"sighup": {
				Val:    slip.Fixnum(1),
				Const:  true,
				Export: true,
				Doc:    "terminate process, terminal line hangup",
			},
			"sigint": {
				Val:    slip.Fixnum(2),
				Const:  true,
				Export: true,
				Doc:    "terminate process, interrupt program",
			},
			"sigquit": {
				Val:    slip.Fixnum(3),
				Const:  true,
				Export: true,
				Doc:    "create core image, quit program",
			},
			"sigill": {
				Val:    slip.Fixnum(4),
				Const:  true,
				Export: true,
				Doc:    "create core image, illegal instruction",
			},
			"sigtrap": {
				Val:    slip.Fixnum(5),
				Const:  true,
				Export: true,
				Doc:    "create core image, trace trap",
			},
			"sigabrt": {
				Val:    slip.Fixnum(6),
				Const:  true,
				Export: true,
				Doc:    "create core image, abort program (formerly SIGIOT)",
			},
			"sigemt": {
				Val:    slip.Fixnum(7),
				Const:  true,
				Export: true,
				Doc:    "create core image, emulate instruction executed",
			},
			"sigfpe": {
				Val:    slip.Fixnum(8),
				Const:  true,
				Export: true,
				Doc:    "create core image, floating-point exception",
			},
			"sigkill": {
				Val:    slip.Fixnum(9),
				Const:  true,
				Export: true,
				Doc:    "terminate process, kill program",
			},
			"sigbus": {
				Val:    slip.Fixnum(10),
				Const:  true,
				Export: true,
				Doc:    "create core image, bus error",
			},
			"sigsegv": {
				Val:    slip.Fixnum(11),
				Const:  true,
				Export: true,
				Doc:    "create core image, segmentation violation",
			},
			"sigsys": {
				Val:    slip.Fixnum(12),
				Const:  true,
				Export: true,
				Doc:    "create core image, non-existent system call invoked",
			},
			"sigpipe": {
				Val:    slip.Fixnum(13),
				Const:  true,
				Export: true,
				Doc:    "terminate process, write on a pipe with no reader",
			},
			"sigalrm": {
				Val:    slip.Fixnum(14),
				Const:  true,
				Export: true,
				Doc:    "terminate process, real-time timer expired",
			},
			"sigterm": {
				Val:    slip.Fixnum(15),
				Const:  true,
				Export: true,
				Doc:    "terminate process, software termination signal",
			},
			"sigurg": {
				Val:    slip.Fixnum(16),
				Const:  true,
				Export: true,
				Doc:    "discard signal, urgent condition present on socket",
			},
			"sigstop": {
				Val:    slip.Fixnum(17),
				Const:  true,
				Export: true,
				Doc:    "stop process, stop (cannot be caught or ignored)",
			},
			"sigtstp": {
				Val:    slip.Fixnum(18),
				Const:  true,
				Export: true,
				Doc:    "stop process, stop signal generated from keyboard",
			},
			"sigcont": {
				Val:    slip.Fixnum(19),
				Const:  true,
				Export: true,
				Doc:    "discard signal, continue after stop",
			},
			"sigchld": {
				Val:    slip.Fixnum(20),
				Const:  true,
				Export: true,
				Doc:    "discard signal, child status has changed",
			},
			"sigttin": {
				Val:    slip.Fixnum(21),
				Const:  true,
				Export: true,
				Doc:    "stop process, background read attempted from control terminal",
			},
			"sigttou": {
				Val:    slip.Fixnum(22),
				Const:  true,
				Export: true,
				Doc:    "stop process, background write attempted to control terminal",
			},
			"sigio": {
				Val:    slip.Fixnum(23),
				Const:  true,
				Export: true,
				Doc:    "discard signal, I/O is possible on a descriptor (see fcntl(2))",
			},
			"sigxcpu": {
				Val:    slip.Fixnum(24),
				Const:  true,
				Export: true,
				Doc:    "terminate process, cpu time limit exceeded (see setrlimit(2))",
			},
			"sigxfsz": {
				Val:    slip.Fixnum(25),
				Const:  true,
				Export: true,
				Doc:    "terminate process, file size limit exceeded (see setrlimit(2))",
			},
			"sigvtalrm": {
				Val:    slip.Fixnum(26),
				Const:  true,
				Export: true,
				Doc:    "terminate process, virtual time alarm (see setitimer(2))",
			},
			"sigprof": {
				Val:    slip.Fixnum(27),
				Const:  true,
				Export: true,
				Doc:    "terminate process, profiling timer alarm (see setitimer(2))",
			},
			"sigwinch": {
				Val:    slip.Fixnum(28),
				Const:  true,
				Export: true,
				Doc:    "discard signal, Window size change",
			},
			"siginfo": {
				Val:    slip.Fixnum(29),
				Const:  true,
				Export: true,
				Doc:    "discard signal, status request from keyboard",
			},
			"january": {
				Val:    slip.Fixnum(1),
				Const:  true,
				Export: true,
				Doc:    "The month of January as a fixnum.",
			},
			"february": {
				Val:    slip.Fixnum(2),
				Const:  true,
				Export: true,
				Doc:    "The month of February as a fixnum.",
			},
			"march": {
				Val:    slip.Fixnum(3),
				Const:  true,
				Export: true,
				Doc:    "The month of March as a fixnum.",
			},
			"april": {
				Val:    slip.Fixnum(4),
				Const:  true,
				Export: true,
				Doc:    "The month of April as a fixnum.",
			},
			"may": {
				Val:    slip.Fixnum(5),
				Const:  true,
				Export: true,
				Doc:    "The month of May as a fixnum.",
			},
			"june": {
				Val:    slip.Fixnum(6),
				Const:  true,
				Export: true,
				Doc:    "The month of June as a fixnum.",
			},
			"july": {
				Val:    slip.Fixnum(7),
				Const:  true,
				Export: true,
				Doc:    "The month of July as a fixnum.",
			},
			"august": {
				Val:    slip.Fixnum(8),
				Const:  true,
				Export: true,
				Doc:    "The month of August as a fixnum.",
			},
			"september": {
				Val:    slip.Fixnum(9),
				Const:  true,
				Export: true,
				Doc:    "The month of September as a fixnum.",
			},
			"october": {
				Val:    slip.Fixnum(10),
				Const:  true,
				Export: true,
				Doc:    "The month of October as a fixnum.",
			},
			"november": {
				Val:    slip.Fixnum(11),
				Const:  true,
				Export: true,
				Doc:    "The month of November as a fixnum.",
			},
			"december": {
				Val:    slip.Fixnum(12),
				Const:  true,
				Export: true,
				Doc:    "The month of December as a fixnum.",
			},
		})
	for _, f := range []*flavors.Flavor{
		defLogger(),
		defSystem(),
	} {
		vv := Pkg.GetVarVal(f.Name())
		vv.Const = true
	}
	Pkg.Initialize(nil, &Env{})

	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}

func seqStarEndArgs(s *slip.Scope, args slip.List, depth int) (start, end int) {
	start = 0
	end = -1
	if v, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":start")); has {
		if num, ok := v.(slip.Fixnum); ok && 0 <= num {
			start = int(num)
		} else {
			slip.TypePanic(s, depth, ":start", v, "fixnum")
		}
	}
	if v, has := slip.GetArgsKeyValue(args[1:], slip.Symbol(":end")); has && v != nil {
		if num, ok := v.(slip.Fixnum); ok && 0 <= num {
			end = int(num)
		} else {
			slip.TypePanic(s, depth, ":start", v, "fixnum")
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
		slip.ErrorPanic(s, depth, "start, %d is greater than length of %d", start, size)
	}
	if size < end {
		slip.ErrorPanic(s, depth, "end, %d is greater than length of %d", end, size)
	}
	if end < start {
		slip.ErrorPanic(s, depth, "end, %d is less start %d", end, start)
	}
	return
}
