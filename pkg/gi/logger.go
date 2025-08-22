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

func defLogger() *flavors.Flavor {
	Pkg.Initialize(nil)
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
		&Pkg,
	)
	logger.DefMethod(":init", "", initCaller{})
	logger.DefMethod(":error", "", errorCaller{})
	logger.DefMethod(":warn", "", warnCaller{})
	logger.DefMethod(":info", "", infoCaller{})
	logger.DefMethod(":debug", "", debugCaller{})
	logger.DefMethod(":log", "", logCaller{})
	logger.DefMethod(":out", "", outCaller{})
	logger.DefMethod(":set-out", "", setOutCaller{})
	logger.DefMethod(":write", "", writeCaller{})
	logger.DefMethod(":shutdown", "", shutdownCaller{})

	return logger
}

// Logger returns the bag-flavor.
func Logger() *flavors.Flavor {
	return logger
}

type initCaller struct{}

func (caller initCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	queue := logQueue{
		queue: make(chan []byte, 100),
		done:  make(chan bool, 1),
	}
	out := slip.StandardOutput
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
						slip.TypePanic(s, depth, ":out", list[i+1], "output-stream")
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

func (caller initCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":init",
		Text: "Sets the output stream.",
		Args: []*slip.DocArg{
			{Name: "&key"},
			{
				Name: ":out",
				Type: "output-stream",
				Text: "Sets the stream logger writes to.",
			},
		},
	}
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

type errorCaller struct{}

func (caller errorCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	logFormat(s, 0, args, true)
	return nil
}

func (caller errorCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":error",
		Text: "Log an error message if the logger _level_ is at or above 0.",
		Args: []*slip.DocArg{
			{
				Name: "format",
				Type: "string",
				Text: "The format string for the message. See __format__.",
			},
			{Name: "&rest"},
			{
				Name: "args*",
				Type: "object",
				Text: "Arguments for the _format_.",
			},
		},
	}
}

type warnCaller struct{}

func (caller warnCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	logFormat(s, 1, args, true)
	return nil
}

func (caller warnCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":warn",
		Text: "Log an warning message if the logger _level_ is at or above 1.",
		Args: []*slip.DocArg{
			{
				Name: "format",
				Type: "string",
				Text: "The format string for the message. See __format__.",
			},
			{Name: "&rest"},
			{
				Name: "args*",
				Type: "object",
				Text: "Arguments for the _format_.",
			},
		},
	}
}

type infoCaller struct{}

func (caller infoCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	logFormat(s, 2, args, true)
	return nil
}

func (caller infoCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":info",
		Text: "Log an informational message if the logger _level_ is at or above 2.",
		Args: []*slip.DocArg{
			{
				Name: "format",
				Type: "string",
				Text: "The format string for the message. See __format__.",
			},
			{Name: "&rest"},
			{
				Name: "args*",
				Type: "object",
				Text: "Arguments for the _format_.",
			},
		},
	}
}

type debugCaller struct{}

func (caller debugCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	logFormat(s, 3, args, true)

	return nil
}

func (caller debugCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":debug",
		Text: "Log a debug message if the logger _level_ is at or above 3.",
		Args: []*slip.DocArg{
			{
				Name: "format",
				Type: "string",
				Text: "The format string for the message. See __format__.",
			},
			{Name: "&rest"},
			{
				Name: "args*",
				Type: "object",
				Text: "Arguments for the _format_.",
			},
		},
	}
}

type logCaller struct{}

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

func (caller logCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":log",
		Text: "Log a message at the specified level ignoring the current logger _level_.",
		Args: []*slip.DocArg{
			{
				Name: "level",
				Type: "fixnum|keyword",
				Text: `Log level of the log entry. Can be a fixnum between 0 and 3
inclusive or :error, :warn, :info, or :debug.`,
			},
			{
				Name: "control",
				Type: "string",
				Text: `Control string for the log entry as in the __format__ function.`,
			},
			{Name: "&rest"},
			{
				Name: "args*",
				Type: "object",
				Text: "Arguments for the _control_ formatting string.",
			},
		},
	}
}

type outCaller struct{}

func (caller outCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	return s.Get("out")
}

func (caller outCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name:   ":out",
		Text:   "Returns the output stream of the logger.",
		Return: "output-stream",
	}
}

type setOutCaller struct{}

func (caller setOutCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	self := s.Get(slip.Symbol("self")).(*flavors.Instance)
	if len(args) != 1 {
		slip.PanicMethodArgChoice(self, ":set-out", len(args), "1")
	}
	switch args[0].(type) {
	case nil:
		self.Let("out", slip.StandardOutput)
	case io.Writer:
		self.Let("out", args[0])
	default:
		slip.TypePanic(s, depth, ":out", args[0], "output-stream")
	}
	return nil
}

func (caller setOutCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":set-out",
		Text: "Sets the logger output stream.",
		Args: []*slip.DocArg{
			{
				Name: "stream",
				Type: "output-stream",
				Text: `The output stream for the logger.`,
			},
		},
	}
}

type writeCaller struct{}

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

func (caller writeCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":write",
		Text: "Write _message_ to _stream_.",
		Args: []*slip.DocArg{
			{
				Name: "message",
				Type: "string",
				Text: `The message to write to the stream.`,
			},
		},
	}
}

type shutdownCaller struct{}

func (caller shutdownCaller) Call(s *slip.Scope, args slip.List, _ int) slip.Object {
	obj := s.Get("self").(*flavors.Instance)
	lg := obj.Any.(*logQueue)
	lg.queue <- nil
	<-lg.done

	return nil
}

func (caller shutdownCaller) FuncDocs() *slip.FuncDoc {
	return &slip.FuncDoc{
		Name: ":shutdown",
		Text: "Shutdown the logger.",
	}
}
