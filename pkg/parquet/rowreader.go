// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/apache/arrow/go/v13/arrow"
	"github.com/apache/arrow/go/v13/arrow/array"
	"github.com/apache/arrow/go/v13/parquet/pqarrow"
	"github.com/ohler55/ojg/oj"
	"github.com/ohler55/slip"
)

type readerRowsCaller bool

const (
	listRow  = 'L'
	assocRow = 'A'
)

type keyColReader struct {
	key    string
	cr     *pqarrow.ColumnReader
	ac     *arrow.Chunked
	chunks []arrow.Array
	ci     int // chunk index
	ccnt   int // chunk len
	format byte

	values slip.List
}

type rowReader struct {
	cra    []*keyColReader
	format byte
}

func (kcr *keyColReader) next() (value any, done bool) {
begin:
	if kcr.ac == nil {
		var err error
		if kcr.ac, err = kcr.cr.NextBatch(batchSize); err != nil {
			panic(err)
		}
		if kcr.ac.Len() == 0 {
			return nil, true
		}
		kcr.chunks = kcr.ac.Chunks()
		kcr.ci = 0
		kcr.ccnt = kcr.chunks[0].Len()
	}
	if kcr.ci < kcr.ccnt {
		value = arrayValue(kcr.chunks[0], kcr.ci, kcr.format)
		kcr.ci++
	} else {
		kcr.chunks = kcr.chunks[1:]
		if 0 < len(kcr.chunks) {
			kcr.ccnt = kcr.chunks[0].Len()
			value = arrayValue(kcr.chunks[0], 0, kcr.format)
			kcr.ci = 1
		} else {
			kcr.ac = nil
			goto begin
		}
	}
	return
}

func newRowReader(fr *pqarrow.FileReader, ids, format slip.Object) *rowReader {
	rr := rowReader{}
	switch format {
	case slip.Symbol(":list"):
		rr.format = listRow
	case slip.Symbol(":assoc"):
		rr.format = assocRow
	default:
		slip.PanicType("format", format, ":list", ":assoc")
	}
	schem, err := fr.Schema()
	if err != nil {
		panic(err)
	}
	switch tids := ids.(type) {
	case nil:
		for i := 0; i < schem.NumFields(); i++ {
			cr, _ := fr.GetColumn(context.Background(), i)
			rr.cra = append(rr.cra, &keyColReader{cr: cr, key: schem.Field(i).Name, format: rr.format})
		}
	case slip.List:
		for _, id := range tids {
			cr, key := findColReader(fr, schem, id)
			if cr == nil {
				slip.NewPanic("column %s not found", id)
			}
			rr.cra = append(rr.cra, &keyColReader{cr: cr, key: key, format: rr.format})
		}
	default:
		slip.PanicType("column-ids", ids, "list", "nil")
	}
	return &rr
}

func (rr *rowReader) next() (row slip.Object) {
	switch rr.format {
	case listRow:
		rlist := make(slip.List, len(rr.cra))
		for i, kcr := range rr.cra {
			value, done := kcr.next()
			if done {
				return
			}
			rlist[i] = slip.SimpleObject(value)
		}
		row = rlist
	case assocRow:
		alist := make(slip.List, len(rr.cra))
		for i, kcr := range rr.cra {
			value, done := kcr.next()
			if done {
				return
			}
			alist[i] = slip.List{slip.String(kcr.key), slip.Tail{Value: slip.SimpleObject(value)}}
		}
		row = alist
	}
	return
}

func findColReader(
	fr *pqarrow.FileReader,
	schem *arrow.Schema,
	id slip.Object) (cr *pqarrow.ColumnReader, name string) {

	switch tid := id.(type) {
	case slip.Fixnum:
		if 0 <= tid && int(tid) < schem.NumFields() {
			cr, _ = fr.GetColumn(context.Background(), int(tid))
			name = schem.Field(int(tid)).Name
		}
	case slip.String:
		if fa := schem.FieldIndices(string(tid)); 0 < len(fa) {
			cr, _ = fr.GetColumn(context.Background(), fa[0])
			name = string(tid)
		}
	case slip.Symbol:
		if fa := schem.FieldIndices(string(tid)); 0 < len(fa) {
			cr, _ = fr.GetColumn(context.Background(), fa[0])
			name = string(tid)
		}
	default:
		slip.PanicType("id", tid, "fixnum", "string", "symbol")
	}
	return
}

// The validity of the index is checked before making this call.
func arrayValue(aa arrow.Array, i int, format byte) (value any) {
	switch tc := aa.(type) {
	case *array.Null: // type having no physical storage
		value = tc.Value(i)
	case *array.Boolean: // is a 1 bit, LSB bit-packed ordering
		value = tc.Value(i)
	case *array.Uint8: // a Unsigned 8-bit little-endian integer
		value = tc.Value(i)
	case *array.Int8: // a Signed 8-bit little-endian integer
		value = tc.Value(i)
	case *array.Uint16: // a Unsigned 16-bit little-endian integer
		value = tc.Value(i)
	case *array.Int16: // a Signed 16-bit little-endian integer
		value = tc.Value(i)
	case *array.Uint32: // a Unsigned 32-bit little-endian integer
		value = tc.Value(i)
	case *array.Int32: // a Signed 32-bit little-endian integer
		value = tc.Value(i)
	case *array.Uint64: // a Unsigned 64-bit little-endian integer
		value = tc.Value(i)
	case *array.Int64: // a Signed 64-bit little-endian integer
		value = tc.Value(i)
	case *array.Float16: // an 2-byte floating point value
		value = tc.Value(i)
	case *array.Float32: // an 4-byte floating point value
		value = tc.Value(i)
	case *array.Float64: // an 8-byte floating point value
		value = tc.Value(i)
	case *array.String: // a UTF8 variable-length string
		value = tc.Value(i)
	case *array.Binary: // a Variable-length byte type (no guarantee of UTF8-ness)
		value = string(tc.Value(i))
	case *array.FixedSizeBinary: // a binary where each value occupies the same number of bytes
		value = string(tc.Value(i))
	case *array.Date32: // int32 days since the UNIX epoch
		value = tc.Value(i).ToTime()
	case *array.Date64: // int64 milliseconds since the UNIX epoch
		value = tc.Value(i).ToTime()
	case *array.Timestamp: // an exact timestamp encoded with int64 since UNIX epoch Default unit millisecond
		tu := arrow.Millisecond
		if at, ok := tc.DataType().(*arrow.TimestampType); ok {
			tu = at.Unit
		}
		value = tc.Value(i).ToTime(tu)
	case *array.Time32: // a signed 32-bit integer, representing either seconds or milliseconds since midnight
		tu := arrow.Millisecond
		if at, ok := tc.DataType().(*arrow.Time32Type); ok {
			tu = at.Unit
		}
		value = tc.Value(i).ToTime(tu)
	case *array.Time64: // a signed 64-bit integer, representing either microseconds or nanoseconds since midnight
		tu := arrow.Nanosecond
		if at, ok := tc.DataType().(*arrow.Time64Type); ok {
			tu = at.Unit
		}
		value = tc.Value(i).ToTime(tu)
	case *array.Map: // a repeated struct logical type
		fmt.Printf("*** amap: %v\n", tc)
		fmt.Printf("*** items: %v\n", tc.Items())
		fmt.Printf("*** 4 marshal: %s\n", tc.GetOneForMarshal(i))

		// TBD return a key/value pair
		//  if kcr, split and use key from kcr
		//  here combine in map ?? need same code to determine a may?

		switch format {
		case listRow:
			value = arrayValue(tc.Items(), i, format)
		case assocRow:
			k := slip.SimpleObject(arrayValue(tc.Keys(), i, format))
			v := slip.SimpleObject(arrayValue(tc.Items(), i, format))
			value = slip.List{k, slip.Tail{Value: v}}
		}
	case *array.Duration: // Measure of elapsed time in either seconds, milliseconds, microseconds or nanoseconds
		value = int64(tc.Value(i))
	default:
		// Includes array.Struct and array.List since there does not appear to
		// be any way to get the data for each item so the horribly
		// inefficient approach is taken of getting the JSON encoded values
		// and parsing.
		value = tc.GetOneForMarshal(i)
		if raw, ok := value.(json.RawMessage); ok {
			value = oj.MustParse([]byte(raw))
		}
	}
	return
}
