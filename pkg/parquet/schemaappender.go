// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"bytes"
	"fmt"
	"strings"

	"github.com/apache/arrow/go/v13/parquet/schema"
)

type schemaAppender struct {
	buf    []byte
	indent int
	pad    int
}

func (sa *schemaAppender) VisitPre(n schema.Node) bool {
	sa.buf = append(sa.buf, bytes.Repeat([]byte{' '}, sa.indent)...)
	if n.Type() == schema.Group {
		g := n.(*schema.GroupNode)
		lt := g.LogicalType()
		ct := g.ConvertedType()
		sa.buf = append(sa.buf, g.RepetitionType().String()...)
		sa.buf = append(sa.buf, " group "...)
		if 0 <= g.FieldID() {
			sa.buf = fmt.Appendf(sa.buf, "field_id=%d ", g.FieldID())
		}
		sa.buf = append(sa.buf, g.Name()...)
		_, invalid := lt.(schema.UnknownLogicalType)
		_, none := lt.(schema.NoLogicalType)

		if lt != nil && !invalid && !none {
			sa.buf = append(sa.buf, ' ', '(')
			sa.buf = append(sa.buf, lt.String()...)
			sa.buf = append(sa.buf, ')')
		} else if ct != schema.ConvertedTypes.None {
			sa.buf = append(sa.buf, ' ', '(')
			sa.buf = append(sa.buf, ct.String()...)
			sa.buf = append(sa.buf, ')')
		}
		sa.buf = append(sa.buf, ' ', '{', '\n')
		sa.indent += sa.pad
	} else {
		p := n.(*schema.PrimitiveNode)
		lt := p.LogicalType()
		ct := p.ConvertedType()
		sa.buf = append(sa.buf, p.RepetitionType().String()...)
		sa.buf = append(sa.buf, ' ')
		sa.buf = append(sa.buf, strings.ToLower(p.PhysicalType().String())...)
		sa.buf = append(sa.buf, ' ')
		if 0 <= p.FieldID() {
			sa.buf = fmt.Appendf(sa.buf, "field_id=%d ", p.FieldID())
		}
		sa.buf = append(sa.buf, p.Name()...)
		_, invalid := lt.(schema.UnknownLogicalType)
		_, none := lt.(schema.NoLogicalType)

		switch {
		case lt != nil && !invalid && !none:
			sa.buf = append(sa.buf, ' ', '(')
			sa.buf = append(sa.buf, lt.String()...)
			sa.buf = append(sa.buf, ')')
		case ct == schema.ConvertedTypes.Decimal:
			sa.buf = fmt.Appendf(sa.buf, " (%s(%d,%d))", ct, p.DecimalMetadata().Precision, p.DecimalMetadata().Scale)
		case ct != schema.ConvertedTypes.None:
			sa.buf = append(sa.buf, ' ', '(')
			sa.buf = append(sa.buf, ct.String()...)
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
