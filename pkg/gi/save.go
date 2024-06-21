// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"bytes"
	"io"
	"math/big"
	"strconv"

	"github.com/ohler55/ojg"
	"github.com/ohler55/slip"
)

const (
	//   0123456789abcdef0123456789abcdef
	needPipeMap = "" +
		"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" + // 0x00
		"xxxxxxxxxx..x..x...........x...." + // 0x20
		"...........................xxx.." + // 0x40
		"x..........................xxx.x" + // 0x60
		"................................" + // 0x80
		"................................" + // 0xa0
		"................................" + // 0xc0
		"................................" //   0xe0

	defaultSaveBufLimit = 65536
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Save{Function: slip.Function{Name: "save", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "save",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to write to the stream.",
				},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
				{Name: "&key"},
				{
					Name: "buffer-size",
					Type: "fixnum",
					Text: "The size of the incremental write buffer.",
				},
			},
			Return: "nil",
			Text: `__save__ _object_ to the _output-stream_. This writes as readable. If
the object can not be written as readable then a panic is raised. The advantage of this
function over a __write__ is that a __write__ build the entire output in memory before
writing while __save__ uses buffered output to write incrementally. __save__ does not
have the breadth of options that __write__ does though.`,
			Examples: []string{
				`(let ((out (make-string-output-stream)))`,
				` (save '(1 2 3) out)`,
				` (get-output-stream-string out)) => "(1 2 3)"`,
			},
		}, &Pkg)
}

// Save represents the save function.
type Save struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Save) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 4)
	w, ok := args[1].(io.Writer)
	if !ok {
		slip.PanicType("output-stream", args[1], "output-stream")
	}
	bsize := defaultSaveBufLimit
	if val, has := slip.GetArgsKeyValue(args[2:], slip.Symbol(":buffer-size")); has {
		if num, ok2 := val.(slip.Fixnum); ok2 && 0 < num {
			bsize = int(num)
		} else {
			slip.PanicType("buffer-size", val, "fixnum")
		}
	}
	b := saveObj(nil, args[0], w, bsize)
	if 0 < len(b) {
		if _, err := w.Write(b); err != nil {
			panic(err)
		}
	}
	return nil
}

func saveObj(b []byte, obj slip.Object, w io.Writer, bsize int) []byte {
Top:
	switch to := obj.(type) {
	case nil:
		b = append(b, "nil"...)
	case slip.List:
		if len(to) == 0 {
			b = append(b, "nil"...)
			return b
		}
		b = append(b, '(')
		for i, element := range to {
			if 0 < i {
				b = append(b, ' ')
			}
			b = saveObj(b, element, w, bsize)
		}
		b = append(b, ')')
	case slip.Symbol:
		if len(to) == 0 {
			b = append(b, "||"...)
			break
		}
		if to[0] == ':' {
			b = append(b, []byte(to)...)
			break
		}
		for _, c := range []byte(to) {
			if needPipeMap[c] == 'x' {
				b = append(b, '|')
				b = append(b, []byte(to)...)
				b = append(b, '|')
				break Top
			}
		}
		b = append(b, []byte(to)...)
	case slip.String:
		b = ojg.AppendJSONString(b, string(to), false)
	case slip.Fixnum:
		b = strconv.AppendInt(b, int64(to), 10)
	case slip.DoubleFloat:
		tmp := strconv.AppendFloat([]byte{}, float64(to), 'e', -1, 64)
		b = append(b, bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'d'})...)
	case slip.Tail:
		b = append(b, '.', ' ')
		obj = to.Value
		goto Top
	case slip.Time:
		b = to.Append(b)
	case slip.SingleFloat:
		tmp := strconv.AppendFloat([]byte{}, float64(to), 'e', -1, 32)
		b = append(b, bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'s'})...)
	case slip.Character:
		b = to.Append(b)
	case *slip.Bignum:
		b = (*big.Int)(to).Append(b, 10)
	case *slip.Ratio:
		if (*big.Rat)(to).IsInt() {
			obj = (*slip.Bignum)((*big.Rat)(to).Num())
			goto Top
		}
		b = (*big.Rat)(to).Num().Append(b, 10)
		b = append(b, '/')
		b = (*big.Rat)(to).Denom().Append(b, 10)
	case *slip.Array:
		obj = to.AsList()
		switch len(to.Dimensions()) {
		case 0:
			b = append(b, "#0A"...)
		default:
			b = append(b, '#')
			b = saveObj(b, slip.Fixnum(len(to.Dimensions())), w, bsize)
			b = append(b, 'A')
		}
		goto Top
	case *slip.Vector:
		obj = to.AsList()
		b = append(b, '#')
		goto Top
	case *slip.LongFloat:
		tmp := (*big.Float)(to).Append([]byte{}, 'e', -1)
		b = append(b, bytes.ReplaceAll(tmp, []byte{'e'}, []byte{'L'})...)
	case *slip.Lambda:
		list := make(slip.List, 0, len(to.Forms)+2)
		list = append(list, slip.Symbol("lambda"))
		args := make(slip.List, 0, len(to.Doc.Args))
		for _, ad := range to.Doc.Args {
			args = append(args, slip.Symbol(ad.Name))
		}
		list = append(list, args)
		list = append(list, to.Forms...)
		obj = list
		goto Top
	default:
		end := len(b)
		b = to.Append(b)
		if end+2 < len(b) && b[end] == '#' && b[end+1] == '<' {
			slip.NewPanic("%s can not be written readably", to)
		}
	}
	if bsize < len(b) {
		if _, err := w.Write(b); err != nil {
			panic(err)
		}
		b = b[:0]
	}
	return b
}
