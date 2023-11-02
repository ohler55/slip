// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"bytes"
	"fmt"
	"strings"

	"golang.org/x/text/cases"
	"golang.org/x/text/language"

	"github.com/apache/arrow/go/v13/parquet"
	"github.com/apache/arrow/go/v13/parquet/schema"
)

type schemaAppender struct {
	buf    []byte
	indent int
	pad    int
}

type primitiveLike interface {
	PhysicalType() parquet.Type
	DecimalMetadata() schema.DecimalMetadata
}

func (sa *schemaAppender) VisitPre(n schema.Node) bool {
	sa.buf = append(sa.buf, bytes.Repeat([]byte{' '}, sa.indent)...)
	lt := n.LogicalType()
	ct := n.ConvertedType()
	invalid := true
	none := true
	if lt != nil {
		_, invalid = lt.(schema.UnknownLogicalType)
		_, none = lt.(schema.NoLogicalType)
	}
	sa.buf = append(sa.buf, n.RepetitionType().String()...)
	if n.Type() == schema.Group {
		sa.buf = append(sa.buf, " group "...)
		if 0 <= n.FieldID() {
			sa.buf = fmt.Appendf(sa.buf, "field_id=%d ", n.FieldID())
		}
		sa.buf = append(sa.buf, n.Name()...)
		if !invalid && !none {
			sa.buf = append(sa.buf, ' ', '(')
			sa.buf = append(sa.buf, lt.String()...)
			sa.buf = append(sa.buf, ')')
		} else if ct != schema.ConvertedTypes.None {
			sa.buf = append(sa.buf, ' ', '(')
			sa.buf = append(sa.buf, cases.Title(language.Und).String(ct.String())...)
			sa.buf = append(sa.buf, ')')
		}
		sa.buf = append(sa.buf, ' ', '{', '\n')
		sa.indent += sa.pad
	} else {
		p := n.(primitiveLike)
		sa.buf = append(sa.buf, ' ')
		sa.buf = append(sa.buf, strings.ToLower(p.PhysicalType().String())...)
		sa.buf = append(sa.buf, ' ')
		if 0 <= n.FieldID() {
			sa.buf = fmt.Appendf(sa.buf, "field_id=%d ", n.FieldID())
		}
		sa.buf = append(sa.buf, n.Name()...)
		switch {
		case !invalid && !none:
			sa.buf = append(sa.buf, ' ', '(')
			sa.buf = append(sa.buf, lt.String()...)
			sa.buf = append(sa.buf, ')')
		case ct == schema.ConvertedTypes.Decimal:
			sa.buf = fmt.Appendf(sa.buf, " (%s(precision=%d, scale=%d))",
				cases.Title(language.Und).String(ct.String()),
				p.DecimalMetadata().Precision, p.DecimalMetadata().Scale)
		case ct != schema.ConvertedTypes.None:
			sa.buf = append(sa.buf, ' ', '(')
			sa.buf = append(sa.buf, cases.Title(language.Und).String(ct.String())...)
			sa.buf = append(sa.buf, ')')
		}
		sa.buf = append(sa.buf, ';', '\n')
	}
	return true
}

func (sa *schemaAppender) VisitPost(n schema.Node) {
	if n.Type() == schema.Group {
		sa.indent -= sa.pad
		sa.buf = append(sa.buf, bytes.Repeat([]byte{' '}, sa.indent)...)
		sa.buf = append(sa.buf, '}', '\n')
	}
}
