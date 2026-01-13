// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"fmt"
	"io"
	"strconv"

	"github.com/ohler55/slip"
)

const headerSize = 6 // 6 hex digits for message length

// ReadWireMessage reads a length-prefixed S-expression from the reader.
// The Swank wire format is: 6 ASCII hex characters (length) + UTF-8 payload.
func ReadWireMessage(r io.Reader, scope *slip.Scope) (slip.Object, error) {
	// Read 6-byte header containing hex-encoded length
	header := make([]byte, headerSize)
	if _, err := io.ReadFull(r, header); err != nil {
		return nil, err
	}

	// Decode length from hex
	length, err := strconv.ParseInt(string(header), 16, 32)
	if err != nil {
		return nil, fmt.Errorf("invalid message header: %s", header)
	}

	// Read payload
	payload := make([]byte, length)
	if _, err := io.ReadFull(r, payload); err != nil {
		return nil, err
	}

	// Parse S-expression using SLIP's reader
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
