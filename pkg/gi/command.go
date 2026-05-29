// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"io"
	"os/exec"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

var commandFlavor *flavors.Flavor

func defCommand() {
	commandFlavor = flavors.DefFlavor("command",
		map[string]slip.Object{},
		[]string{},
		slip.List{
			slip.List{
				slip.Symbol(":init-keywords"),
				slip.Symbol(`:path`),
				slip.Symbol(`:args`),
				slip.Symbol(`:dir`),
			},
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`Represents a command to start a process.`),
			},
		},
		&Pkg,
	)
	commandFlavor.DefMethod(":init", ":after", commandInitCaller{})

	commandFlavor.DefMethod(":path", "", commandPathCaller{})
	flavors.FlosFun("command-path", ":path", commandPathCaller{}.FuncDocs(), &Pkg)
	commandFlavor.DefMethod(":set-path", "", commandSetPathCaller{})
	flavors.FlosFun("command-set-path", ":set-path", commandSetPathCaller{}.FuncDocs(), &Pkg)

	commandFlavor.DefMethod(":args", "", commandArgsCaller{})
	flavors.FlosFun("command-args", ":args", commandArgsCaller{}.FuncDocs(), &Pkg)
	commandFlavor.DefMethod(":set-args", "", commandSetArgsCaller{})
	flavors.FlosFun("command-set-args", ":set-args", commandSetArgsCaller{}.FuncDocs(), &Pkg)

	commandFlavor.DefMethod(":dir", "", commandDirCaller{})
	flavors.FlosFun("command-dir", ":dir", commandDirCaller{}.FuncDocs(), &Pkg)
	commandFlavor.DefMethod(":set-dir", "", commandSetDirCaller{})
	flavors.FlosFun("command-set-dir", ":set-dir", commandSetDirCaller{}.FuncDocs(), &Pkg)

	commandFlavor.DefMethod(":env", "", commandEnvCaller{})
	flavors.FlosFun("command-env", ":env", commandEnvCaller{}.FuncDocs(), &Pkg)
	commandFlavor.DefMethod(":set-env", "", commandSetEnvCaller{})
	flavors.FlosFun("command-set-env", ":set-env", commandSetEnvCaller{}.FuncDocs(), &Pkg)

	commandFlavor.DefMethod(":stdout", "", commandStdoutCaller{})
	flavors.FlosFun("command-stdout", ":stdout", commandStdoutCaller{}.FuncDocs(), &Pkg)
	commandFlavor.DefMethod(":set-stdout", "", commandSetStdoutCaller{})
	flavors.FlosFun("command-set-stdout", ":set-stdout", commandSetStdoutCaller{}.FuncDocs(), &Pkg)

	commandFlavor.DefMethod(":stderr", "", commandStderrCaller{})
	flavors.FlosFun("command-stderr", ":stderr", commandStderrCaller{}.FuncDocs(), &Pkg)
	commandFlavor.DefMethod(":set-stderr", "", commandSetStderrCaller{})
	flavors.FlosFun("command-set-stderr", ":set-stderr", commandSetStderrCaller{}.FuncDocs(), &Pkg)

	commandFlavor.DefMethod(":stdin", "", commandStdinCaller{})
	flavors.FlosFun("command-stdin", ":stdin", commandStdinCaller{}.FuncDocs(), &Pkg)
	commandFlavor.DefMethod(":set-stdin", "", commandSetStdinCaller{})
	flavors.FlosFun("command-set-stdin", ":set-stdin", commandSetStdinCaller{}.FuncDocs(), &Pkg)

	commandFlavor.DefMethod(":run", "", commandRunCaller{})
	flavors.FlosFun("command-run", ":run", commandRunCaller{}.FuncDocs(), &Pkg)

	commandFlavor.DefMethod(":start", "", commandStartCaller{})
	flavors.FlosFun("command-start", ":start", commandStartCaller{}.FuncDocs(), &Pkg)

	commandFlavor.DefMethod(":pid", "", commandPidCaller{})
	flavors.FlosFun("command-pid", ":pid", commandPidCaller{}.FuncDocs(), &Pkg)

	commandFlavor.DefMethod(":process", "", commandProcessCaller{})
	flavors.FlosFun("command-process", ":process", commandProcessCaller{}.FuncDocs(), &Pkg)
}

type commandInitCaller struct{}

func (caller commandInitCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	if 0 < len(args) {
		args = args[0].(slip.List)
	}
	var (
		path  string
		dir   string
		cargs []string
	)
	for i := 0; i < len(args)-1; i += 2 {
		sym := args[i].(slip.Symbol)
		key := strings.ToLower(string(sym))
		value := args[i+1]
		switch key {
		case ":path":
			path = slip.MustBeString(value, ":path")
		case ":args":
			if list, ok := value.(slip.List); ok {
				cargs = make([]string, len(list))
				for j, v := range list {
					cargs[j] = slip.MustBeString(v, ":args")
				}
			} else {
				slip.TypePanic(s, depth, ":args", value, "list")
			}
		case ":dir":
			dir = slip.MustBeString(value, ":dir")
		}
	}
	c := exec.Command(path, cargs...)
	if 0 < len(dir) {
		c.Dir = dir
	}
	self.Any = c

	return nil
}

func (caller commandInitCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":init",
		Args: []*slip.DocArg{
			{Name: "&key"},
			{
				Name: "path",
				Type: "string",
				Text: "The path to the command to run.",
			},
			{
				Name: "args",
				Type: "list",
				Text: "The command line argument to command as a list of strings.",
			},
			{
				Name: "dir",
				Type: "string",
				Text: "The working directory to execute the command in.",
			},
		},
		Text: "Initializes the path, args, and dir variables of the command.",
		Kind: slip.MethodSymbol,
	}
}

type commandPathCaller struct{}

func (caller commandPathCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":path", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)

	return slip.String(command.Path)
}

func (caller commandPathCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":path",
		Text:   `Returns the command path.`,
		Kind:   slip.MethodSymbol,
		Return: "string",
	}
}

type commandSetPathCaller struct{}

func (caller commandSetPathCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":set-path", len(args), 1, 1)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		slip.ErrorPanic(s, depth, "Can not set path after :run or :start.")
	}
	command.Path = slip.MustBeString(args[0], "path")

	return args[0]
}

func (caller commandSetPathCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":set-path",
		Args: []*slip.DocArg{
			{
				Name: "path",
				Type: "string",
				Text: "The path to the command to run.",
			},
		},
		Text:   `Sets the command path.`,
		Kind:   slip.MethodSymbol,
		Return: "string",
	}
}

type commandArgsCaller struct{}

func (caller commandArgsCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":args", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)
	list := make(slip.List, len(command.Args))
	for i, str := range command.Args {
		list[i] = slip.String(str)
	}
	return list
}

func (caller commandArgsCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":args",
		Text:   `Returns the command args.`,
		Kind:   slip.MethodSymbol,
		Return: "list",
	}
}

type commandSetArgsCaller struct{}

func (caller commandSetArgsCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":set-args", len(args), 1, 1)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		slip.ErrorPanic(s, depth, "Can not set args after :run or :start.")
	}
	if list, ok := args[0].(slip.List); ok {
		command.Args = make([]string, len(list))
		for j, v := range list {
			command.Args[j] = slip.MustBeString(v, "args")
		}
	} else {
		slip.TypePanic(s, depth, "args", args[0], "list")
	}
	return args[0]
}

func (caller commandSetArgsCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":set-args",
		Args: []*slip.DocArg{
			{
				Name: "args",
				Type: "list",
				Text: "The args to the command to run.",
			},
		},
		Text:   `Sets the command args.`,
		Kind:   slip.MethodSymbol,
		Return: "list",
	}
}

type commandDirCaller struct{}

func (caller commandDirCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":dir", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)

	return slip.String(command.Dir)
}

func (caller commandDirCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":dir",
		Text:   `Returns the command dir.`,
		Kind:   slip.MethodSymbol,
		Return: "string",
	}
}

type commandSetDirCaller struct{}

func (caller commandSetDirCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":set-dir", len(args), 1, 1)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		slip.ErrorPanic(s, depth, "Can not set dir after :run or :start.")
	}
	command.Dir = slip.MustBeString(args[0], "dir")

	return args[0]
}

func (caller commandSetDirCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":set-dir",
		Args: []*slip.DocArg{
			{
				Name: "dir",
				Type: "string",
				Text: "The dir to run the command in.",
			},
		},
		Text:   `Sets the command dir.`,
		Kind:   slip.MethodSymbol,
		Return: "string",
	}
}

type commandEnvCaller struct{}

func (caller commandEnvCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":env", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)
	list := make(slip.List, len(command.Env))
	for i, str := range command.Env {
		list[i] = slip.String(str)
	}
	return list
}

func (caller commandEnvCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":env",
		Text:   `Returns the command env.`,
		Kind:   slip.MethodSymbol,
		Return: "list",
	}
}

type commandSetEnvCaller struct{}

func (caller commandSetEnvCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":set-env", len(args), 1, 1)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		slip.ErrorPanic(s, depth, "Can not set env after :run or :start.")
	}
	if list, ok := args[0].(slip.List); ok {
		command.Env = make([]string, len(list))
		for j, v := range list {
			command.Env[j] = slip.MustBeString(v, "env")
		}
	} else {
		slip.TypePanic(s, depth, "env", args[0], "list")
	}
	return args[0]
}

func (caller commandSetEnvCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":set-env",
		Args: []*slip.DocArg{
			{
				Name: "env",
				Type: "list",
				Text: "The env of the command.",
			},
		},
		Text:   `Sets the command env.`,
		Kind:   slip.MethodSymbol,
		Return: "list",
	}
}

type commandStdoutCaller struct{}

func (caller commandStdoutCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":stdout", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)

	switch tout := command.Stdout.(type) {
	case *slip.OutputStream:
		result = tout
	case nil:
		// leave result as nil
	default:
		result = &slip.OutputStream{Writer: tout}
	}
	return
}

func (caller commandStdoutCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":stdout",
		Text:   `Returns the command stdout.`,
		Kind:   slip.MethodSymbol,
		Return: "output-stream",
	}
}

type commandSetStdoutCaller struct{}

func (caller commandSetStdoutCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":set-stdout", len(args), 1, 1)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		slip.ErrorPanic(s, depth, "Can not set stdout after :run or :start.")
	}
	if w, ok := args[0].(io.Writer); ok {
		command.Stdout = w
	} else {
		slip.TypePanic(s, depth, "stream", args[0], "output-stream")
	}
	return args[0]
}

func (caller commandSetStdoutCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":set-stdout",
		Args: []*slip.DocArg{
			{
				Name: "stream",
				Type: "output-stream",
				Text: "The stream to set the command stdout.",
			},
		},
		Text:   `Sets the command stdout.`,
		Kind:   slip.MethodSymbol,
		Return: "output-stream",
	}
}

type commandStderrCaller struct{}

func (caller commandStderrCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":stderr", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)

	switch terr := command.Stderr.(type) {
	case *slip.OutputStream:
		result = terr
	case nil:
		// leave result as nil
	default:
		result = &slip.OutputStream{Writer: terr}
	}
	return
}

func (caller commandStderrCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":stderr",
		Text:   `Returns the command stderr.`,
		Kind:   slip.MethodSymbol,
		Return: "output-stream",
	}
}

type commandSetStderrCaller struct{}

func (caller commandSetStderrCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":set-stderr", len(args), 1, 1)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		slip.ErrorPanic(s, depth, "Can not set stderr after :run or :start.")
	}
	if w, ok := args[0].(io.Writer); ok {
		command.Stderr = w
	} else {
		slip.TypePanic(s, depth, "stream", args[0], "output-stream")
	}
	return args[0]
}

func (caller commandSetStderrCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":set-stderr",
		Args: []*slip.DocArg{
			{
				Name: "stream",
				Type: "output-stream",
				Text: "The stream to set the command stderr.",
			},
		},
		Text:   `Sets the command stderr.`,
		Kind:   slip.MethodSymbol,
		Return: "output-stream",
	}
}

type commandStdinCaller struct{}

func (caller commandStdinCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":stdin", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)

	switch tin := command.Stdin.(type) {
	case *slip.InputStream:
		result = tin
	case nil:
		// leave result as nil
	default:
		r := slip.InputStream{}
		r.Reader = tin
		result = &r
	}
	return
}

func (caller commandStdinCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":stdin",
		Text:   `Returns the command stdin.`,
		Kind:   slip.MethodSymbol,
		Return: "input-stream",
	}
}

type commandSetStdinCaller struct{}

func (caller commandSetStdinCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":set-stdin", len(args), 1, 1)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		slip.ErrorPanic(s, depth, "Can not set stdin after :run or :start.")
	}
	if r, ok := args[0].(io.Reader); ok {
		command.Stdin = r
	} else {
		slip.TypePanic(s, depth, "stream", args[0], "input-stream")
	}
	return args[0]
}

func (caller commandSetStdinCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":set-stdin",
		Args: []*slip.DocArg{
			{
				Name: "stream",
				Type: "input-stream",
				Text: "The stream to set the command stdin.",
			},
		},
		Text:   `Sets the command stdin.`,
		Kind:   slip.MethodSymbol,
		Return: "input-stream",
	}
}

type commandRunCaller struct{}

func (caller commandRunCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":run", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		slip.ErrorPanic(s, depth, "Already running or already ran.")
	}
	if err := command.Run(); err != nil {
		slip.ErrorPanic(s, depth, ":run error. %s", err)
	}
	return nil
}

func (caller commandRunCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":run",
		Text:   `Runs the command and wait for completion. Panics on error.`,
		Kind:   slip.MethodSymbol,
		Return: "nil",
	}
}

type commandStartCaller struct{}

func (caller commandStartCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":start", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		slip.ErrorPanic(s, depth, "Already running or already ran.")
	}
	if err := command.Start(); err != nil {
		slip.ErrorPanic(s, depth, ":start error. %s", err)
	}
	return nil
}

func (caller commandStartCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":start",
		Text:   `Starts the command. Panics on error.`,
		Kind:   slip.MethodSymbol,
		Return: "nil",
	}
}

type commandPidCaller struct{}

func (caller commandPidCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":pid", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		result = slip.Fixnum(command.Process.Pid)
	}
	return
}

func (caller commandPidCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":pid",
		Text:   `Returns the command pid.`,
		Kind:   slip.MethodSymbol,
		Return: "fixnum",
	}
}

type commandProcessCaller struct{}

func (caller commandProcessCaller) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	self := s.Get("self").(*flavors.Instance)
	slip.MethodArgCountCheck(s, depth, self, ":process", len(args), 0, 0)
	command := self.Any.(*exec.Cmd)
	if command.Process != nil {
		inst := commandFlavor.MakeInstance().(*flavors.Instance)
		inst.Any = command.Process
		result = inst
	}
	return
}

func (caller commandProcessCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":process",
		Text:   `Returns the command process.`,
		Kind:   slip.MethodSymbol,
		Return: "process",
	}
}
