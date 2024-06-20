// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"bufio"
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
)

type bufWriter struct {
	w *bufio.Writer
}

func (bw *bufWriter) Write(b []byte) {
	if _, err := bw.w.Write(b); err != nil {
		panic(err)
	}
}

func (bw *bufWriter) WriteByte(b byte) {
	if err := bw.w.WriteByte(b); err != nil {
		panic(err)
	}
}

func (bw *bufWriter) Flush() {
	if err := bw.w.Flush(); err != nil {
		panic(err)
	}
}

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
			},
			Return: "nil",
			Text: `__save__ _object_ to the _output-stream_. This writes as readable. If the object
can not be written as readable then a panic is raised.`,
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
	slip.ArgCountCheck(f, args, 2, 2)
	w, ok := args[1].(io.Writer)
	if !ok {
		slip.PanicType("output-stream", args[1], "output-stream")
	}
	bw := &bufWriter{
		w: bufio.NewWriter(w),
	}
	saveToWriter(bw, args[0])
	bw.Flush()

	return nil
}

func saveToWriter(bw *bufWriter, obj slip.Object) {
	var err error
Top:
	switch to := obj.(type) {
	case nil:
		bw.Write([]byte("nil"))
	case slip.Character:
		bw.Write(to.Append(nil))
	case slip.Fixnum:
		bw.Write(strconv.AppendInt(nil, int64(to), 10))
	case *slip.Bignum:
		bw.Write((*big.Int)(to).Append(nil, 10))
	case *slip.Ratio:
		if (*big.Rat)(to).IsInt() {
			obj = (*slip.Bignum)((*big.Rat)(to).Num())
			goto Top
		}
		bw.Write((*big.Rat)(to).Num().Append(nil, 10))
		bw.WriteByte('/')
		bw.Write((*big.Rat)(to).Denom().Append(nil, 10))
	case slip.SingleFloat:
		tmp := strconv.AppendFloat([]byte{}, float64(to), 'e', -1, 32)
		bw.Write(bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'s'}))

	case slip.DoubleFloat:
		tmp := strconv.AppendFloat([]byte{}, float64(to), 'e', -1, 64)
		bw.Write(bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'d'}))
	case *slip.LongFloat:
		tmp := (*big.Float)(to).Append([]byte{}, 'e', -1)
		bw.Write(bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'L'}))
	case slip.Symbol:
		if len(to) == 0 {
			bw.Write([]byte("||"))
			break
		}
		if to[0] == ':' {
			bw.Write([]byte(to))
			break
		}
		for _, c := range []byte(to) {
			if needPipeMap[c] == 'x' {
				bw.WriteByte('|')
				bw.Write([]byte(to))
				bw.WriteByte('|')
				break Top
			}
		}
		bw.Write([]byte(to))
	case slip.String:
		bw.Write(ojg.AppendJSONString(nil, string(to), false))
	case slip.List:
		if len(to) == 0 {
			bw.Write([]byte("nil"))
			return
		}
		bw.WriteByte('(')
		for i, element := range to {
			// TBD handle Tail
			if 0 < i {
				bw.WriteByte(' ')
			}
			saveToWriter(bw, element)
		}
		bw.WriteByte(')')
	case *slip.Array:
		obj = to.AsList()
		switch len(to.Dimensions()) {
		case 0:
			bw.Write([]byte("#0A"))
		case 1:
			bw.WriteByte('#')
		default:
			bw.WriteByte('#')
			saveToWriter(bw, slip.Fixnum(len(to.Dimensions())))
			bw.WriteByte('A')
		}
		goto Top
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
	case slip.SpecialSyntax:
		bw.Write([]byte(to.SpecialPrefix()))
		obj = to.GetArgs()[0]
		goto Top
	case slip.Funky:
		name := to.GetName()
		obj = append(slip.List{slip.Symbol(name)}, to.GetArgs()...)
		goto Top
	default:
		b := to.Append(nil)
		if bytes.HasPrefix(b, []byte("#<")) {
			slip.NewPanic("%s can not be written readably", to)
		}
		bw.Write(b)
	}
	if err != nil {
		panic(err)
	}
}
