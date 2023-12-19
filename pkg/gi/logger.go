// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"io"
	"time"

	"github.com/ohler55/ojg"
	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/pkg/flavors"
)

type logQueue struct {
	queue chan []byte
	done  chan bool
}

var logger *flavors.Flavor

var colorCodes = []string{
	"\x1b[31m",
	"\x1b[33m",
	"",
	"\x1b[35m",
}

func init() {
	logger = flavors.DefFlavor(
		"logger-flavor",
		map[string]slip.Object{ // instance variables
			"level":      slip.Fixnum(1),
			"with-time":  nil, // boolean
			"with-level": slip.True,
			"colorize":   nil,
			"json":       nil,
		},
		nil, // inherit
		slip.List{ // options
			slip.Symbol(":gettable-instance-variables"),
			slip.Symbol(":settable-instance-variables"),
			slip.Symbol(":inittable-instance-variables"),
			slip.List{
				slip.Symbol(":default-init-plist"),
				slip.List{
					slip.Symbol(":out"),
					nil,
				},
			},
			slip.List{
				slip.Symbol(":documentation"),
				slip.String(`Logs messages to a stream with various options.`),
			},
		},
	)
	logger.DefMethod(":init", "", initCaller(true))
	logger.DefMethod(":error", "", errorCaller(true))
	logger.DefMethod(":warn", "", warnCaller(true))
	logger.DefMethod(":info", "", infoCaller(true))
	logger.DefMethod(":debug", "", debugCaller(true))
	logger.DefMethod(":log", "", logCaller(true))
	logger.DefMethod(":out", "", outCaller(true))
	logger.DefMethod(":set-out", "", setOutCaller(true))
	logger.DefMethod(":write", "", writeCaller(true))
	logger.DefMethod(":shutdown", "", shutdownCaller(true))
}

// Logger returns the bag-flavor.
func Logger() *flavors.Flavor {
	return logger
}

type initCaller bool

func (caller initCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	queue := logQueue{
		queue: make(chan []byte, 100),
		done:  make(chan bool, 1),
	}
	var out slip.Object = slip.StandardOutput
	if 0 < len(args) {
		if list, ok := args[0].(slip.List); ok {
			for i := 0; i < len(list)-1; i++ {
				if slip.Symbol(":out") == list[i] {
					switch list[i+1].(type) {
					case nil:
						out = slip.StandardOutput
					case io.Writer:
						out = list[i+1]
					default:
						slip.PanicType(":out", list[i+1], "output-stream")
					}
				}
			}
		}
	}
	obj.Let("out", out)
	obj.Any = &queue
	go func() {
		wargs := slip.List{nil}
		for {
			message := <-queue.queue
			if len(message) == 0 {
				break
			}
			wargs[0] = slip.String(message)
			obj.Receive(s, ":write", wargs, 0)
		}
		queue.done <- true
	}()
	return nil
}

func (caller initCaller) Docs() string {
	return `__:init__`
}

func logFormat(s *slip.Scope, level int, args slip.List, check bool) {
	if check {
		limit, _ := s.Get("level").(slip.Fixnum)
		if int(limit) < level {
			return
		}
	}
	asJSON := s.Get("json") != nil
	color := s.Get("colorize") != nil
	message := cl.FormatArgs(s, args)
	var buf []byte
	if asJSON {
		entry := map[string]any{
			"when":    time.Now().UTC().Format(time.RFC3339Nano),
			"level":   []string{"error", "warn", "info", "debug"}[level],
			"message": string(message),
		}
		buf = append(buf, oj.JSON(entry, &ojg.Options{Sort: true})...)
	} else {
		if s.Get("with-time") != nil {
			buf = time.Now().UTC().AppendFormat(buf, time.RFC3339Nano)
			buf = append(buf, ' ')
		}
		if color {
			buf = append(buf, colorCodes[level]...)
		}
		if s.Get("with-level") != nil {
			buf = append(buf, "EWID"[level])
			buf = append(buf, ' ')
		}
		buf = append(buf, message...)
		if color {
			buf = append(buf, "\x1b[m"...)
		}
	}
	obj := s.Get("self").(*flavors.Instance)
	obj.Any.(*logQueue).queue <- buf
}

type errorCaller bool

func (caller errorCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	logFormat(s, 0, args, true)
	return nil
}

func (caller errorCaller) Docs() string {
	return `__:error__ _format_ &rest _args_


Log an error message if the logger _level_ is at or above 0.
`
}

type warnCaller bool

func (caller warnCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	logFormat(s, 1, args, true)
	return nil
}

func (caller warnCaller) Docs() string {
	return `__:warn__ _format_ &rest _args_


Log a warn message if the logger _level_ is at or above 1.
`
}

type infoCaller bool

func (caller infoCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	logFormat(s, 2, args, true)
	return nil
}

func (caller infoCaller) Docs() string {
	return `__:info__ _format_ &rest _args_


Log an info message if the logger _level_ is at or above 2.
`
}

type debugCaller bool

func (caller debugCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	logFormat(s, 3, args, true)

	return nil
}

func (caller debugCaller) Docs() string {
	return `__:debug__ _format_ &rest _args_


Log a debug message if the logger _level_ is at or above 3.
`
}

type logCaller bool

func (caller logCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	level := 0
	switch ta := args[0].(type) {
	case slip.Fixnum:
		level = int(ta)
		if level < 0 {
			level = 0
		} else if 3 < level {
			level = 3
		}
	case slip.Symbol:
		switch ta {
		case slip.Symbol("error"), slip.Symbol(":error"):
			level = 0
		case slip.Symbol("warn"), slip.Symbol(":warn"):
			level = 1
		case slip.Symbol("info"), slip.Symbol(":info"):
			level = 2
		case slip.Symbol("debug"), slip.Symbol(":debug"):
			level = 3
		}
	}
	logFormat(s, level, args[1:], false)

	return nil
}

func (caller logCaller) Docs() string {
	return `__:log__ _level_ _control_ &rest _args_
   _level_ [fixnum|symbol] of the log entry. Can be a fixnum between 0 and 3
inclusive or :error, :warn, :info, or :debug.
   _control_ [string] of the the log entry where _control_ is the control string to the __format__ function.
   _rest_ of the arguments to the _control_ string.


Log a message at the specified level ignoring the current logger _level_.
`
}

type outCaller bool

func (caller outCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	return s.Get("out")
}

func (caller outCaller) Docs() string {
	return `__:out__


Returns the output stream.
`
}

type setOutCaller bool

func (caller setOutCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	self := s.Get(slip.Symbol("self")).(*flavors.Instance)
	if len(args) != 1 {
		flavors.PanicMethodArgChoice(self, ":set-out", len(args), "1")
	}
	switch args[0].(type) {
	case nil:
		self.Let("out", slip.StandardOutput)
	case io.Writer:
		self.Let("out", args[0])
	default:
		slip.PanicType(":out", args[0], "output-stream")
	}
	return nil
}

func (caller setOutCaller) Docs() string {
	return `__:set-out__


Sets the output stream.
`
}

type writeCaller bool

func (caller writeCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	out := s.Get("out")
	if w, ok := out.(io.Writer); ok {
		var message slip.String
		if message, ok = args[0].(slip.String); ok {
			_, _ = w.Write(append([]byte(message), '\n'))
		}
	}
	return nil
}

func (caller writeCaller) Docs() string {
	return `__:write__ _message_ => _nil_
  _message_ The message to write to the stream.


Write _message_ to _stream_.
`
}

type shutdownCaller bool

func (caller shutdownCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	lg := obj.Any.(*logQueue)
	lg.queue <- nil
	<-lg.done

	return nil
}

func (caller shutdownCaller) Docs() string {
	return `__:shutdown__


Shutdown the logger.
`
}
