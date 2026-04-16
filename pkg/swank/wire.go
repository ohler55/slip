// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"fmt"
	"io"
	"strconv"

	"github.com/ohler55/slip"
)

const (
	headerSize     = 6                // 6 hex digits for message length
	maxMessageSize = 16 * 1024 * 1024 // 16 MiB upper bound on payload length
)

// ReadWireMessage reads a length-prefixed S-expression from the reader.
// The Swank wire format is: 6 ASCII hex characters (length) + UTF-8 payload.
func ReadWireMessage(r io.Reader, scope *slip.Scope) (obj slip.Object, err error) {
	// Recover from any unexpected panic below (parse panics, allocation
	// panics from a malicious header, etc). Registered before any work so
	// it covers the full body.
	defer func() {
		if rec := recover(); rec != nil {
			err = fmt.Errorf("parse error: %v", rec)
			obj = nil
		}
	}()

	// Read 6-byte header containing hex-encoded length.
	header := make([]byte, headerSize)
	if _, err := io.ReadFull(r, header); err != nil {
		return nil, err
	}

	// ParseUint rejects a leading sign, so `-0001F` cannot reach make().
	length, err := strconv.ParseUint(string(header), 16, 32)
	if err != nil {
		return nil, fmt.Errorf("invalid message header: %s", header)
	}
	if length > maxMessageSize {
		return nil, fmt.Errorf("message length %d exceeds max %d", length, maxMessageSize)
	}

	// Read payload
	payload := make([]byte, length)
	if _, err := io.ReadFull(r, payload); err != nil {
		return nil, err
	}

	code := slip.Read(payload, scope)
	if len(code) == 0 {
		return nil, nil
	}
	return code[0], nil
}

// WriteWireMessage writes a length-prefixed S-expression to the writer.
func WriteWireMessage(w io.Writer, msg slip.Object) error {
	// Serialize to S-expression string
	payload := slip.ObjectString(msg)

	// Encode length as 6 uppercase hex digits
	header := fmt.Sprintf("%06X", len(payload))

	// Write header + payload
	if _, err := w.Write([]byte(header)); err != nil {
		return err
	}
	_, err := w.Write([]byte(payload))
	return err
}
