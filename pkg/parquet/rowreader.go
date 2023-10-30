// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"context"

	"github.com/apache/arrow/go/v13/arrow"
	"github.com/apache/arrow/go/v13/parquet/pqarrow"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

type readerRowsCaller bool

const (
	listRow  = 'L'
	bagRow   = 'B'
	assocRow = 'A'
)

type keyColReader struct {
	key    string
	cr     *pqarrow.ColumnReader
	values slip.List
}

type rowReader struct {
	cra    []*keyColReader
	format byte
}

func (kcr *keyColReader) next() (value slip.Object, done bool) {
	if 0 < len(kcr.values) {
		value = kcr.values[0]
		// May not be needed but in case go loses the ref on resize this
		// allows the value to be GCed
		kcr.values[0] = nil
		kcr.values = kcr.values[1:]
		return
	}
	ac, err := kcr.cr.NextBatch(batchSize) // *arrow.Chunked
	if err != nil {
		panic(err)
	}
	if 0 < ac.Len() {
		for _, chunk := range ac.Chunks() {
			kcr.values = arrayToLisp(kcr.values, chunk)
		}
	} else {
		done = true
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
	case slip.Symbol(":bag"):
		rr.format = bagRow
	default:
		slip.PanicType("format", format, ":list", ":assoc", ":bag")
	}
	schem, err := fr.Schema()
	if err != nil {
		panic(err)
	}
	switch tids := ids.(type) {
	case nil:
		for i := 0; i < schem.NumFields(); i++ {
			cr, _ := fr.GetColumn(context.Background(), i)
			rr.cra = append(rr.cra, &keyColReader{cr: cr, key: schem.Field(i).Name})
		}
	case slip.List:
		for _, id := range tids {
			cr, key := findColReader(fr, schem, id)
			if cr == nil {
				slip.NewPanic("column %s not found", id)
			}
			rr.cra = append(rr.cra, &keyColReader{cr: cr, key: key})
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
			rlist[i] = value
		}
		row = rlist
	case assocRow:
		alist := make(slip.List, len(rr.cra))
		for i, kcr := range rr.cra {
			value, done := kcr.next()
			if done {
				return
			}
			alist[i] = slip.List{slip.String(kcr.key), slip.Tail{Value: value}}
		}
		row = alist
	case bagRow:
		m := map[string]any{}
		for _, kcr := range rr.cra {
			value, done := kcr.next()
			if done {
				return
			}
			m[kcr.key] = slip.Simplify(value)
		}
		inst := schemaFlavor.MakeInstance().(*flavors.Instance)
		inst.Any = m
		row = inst
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
