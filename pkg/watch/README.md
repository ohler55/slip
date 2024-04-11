# SLIP Watch package

The watch protocol is simple. Once a socket is open s-expressions are
exchanged. Each expression must consist of a verb, and identifier and
optional value. The verbs supported are different for the server and
client. In each case the verb is the same as a method on the receiver.
