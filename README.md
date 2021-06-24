mason
=====

There are sooo many JSON libraries.

This one is geared towards logging. As such, it aims to handle some of the
conventional pseudo-types (such as string() and mfa()), and to decorate
e.g. pid() with meta info.

Mappings erlang -> JSON.
===

A map() maps to an Object.

     map() -> Object.`

A list() should map to an Array. There are (at least) two special types of
lists that are used as pseudo-types.

  * A string(), i.e. a list of characters, where a character is an integer in
    some range. The range can be e.g. unicode code points, latin-1, ascii,
    etc.
    `[printable()]` -> String.
  * A proplist(), an assoc list expressed as a list of 2-tuples.
    `[{atom(), term()}` -> Object.
  * An ordered set of terms.
    `list()` -> Array.

A tuple() should map to an Array. However, there are some special tuples that
are, by convention, used as pseudo-types.

  * An MFA,
    `{atom(Module), atom(Function), integer(Arity)}` -> String.
  * An IP number,
    `{byte(), byte(), byte(), byte()}` -> String.
  * An IP:Port.
    `{{byte(), byte(), byte(), byte()}, long()}` -> String.
  * An element in a stack dump.
    `{atom(), atom(), integer(), [_, {line, integer()}]}` -> String.
  * A datetime().
    `{{int(Year), int(Month), int(Day)}, {int(H), int(M), int(S)}}` -> String.
  * An ordered set of terms.
    `tuple()` -> Array.

An atom() should map to a String. There are three atoms that are by convention
used as booleans (`true` and `false`) and null (`undefined`).

  * `true` -> true.
  * `false` -> false.
  * `undefined` -> null.
  * `atom()` -> String.

There are two kinds of numbers. They should map to Number. However, an
integer() can represent a timestamp, which should be mapped to a String.
  * `timestamp()` -> String.
  * `float()` -> Number.
  * `integer()` -> Number.

A binary() should map to a String. A binary is often used to hold UTF8 encoded
text, which should map to text. It can also hold a bitstring, which should map
to a hexstring (like "0x1f").

  * `binary()` -> String.

A pid() is mapped to a String. We decorate with the name of the process, where
the name is either the registered name, or the initial call.

  * `pid()`-> String.

A reference is mapped to a String.

  * `reference()` -> String.

A function() is mapped to a String, using the MFA format.

  * `function()` -> String.

A port is mapped to a String. We decorate with the name of the port, and some
context dependent information about the port. E.g., if it's a socket, we add
the local IP:Port.

  * `port()` -> String.


Mappings JSON -> erlang.
===

An Object can be mapped to a map() or a proplist(). The keys (which in JSON
must be String) can be mapped to binary(), atom(), or string().

```Object -> map(atom(), term()) |
             map(string(), term()) |
             map(binary(), term()) |
             list(atom(), term()).
```

An Array can map to a tuple() or a list().

```Array -> tuple() |
            list().
```

A String can map to a string(), a binary(), or an atom(). If the string is a
hexstring, it can be mapped to a bitstring().

```String -> string() |
             binary() |
             bitstring() |
             atom().
```

A number can map to an integer() or a float(). We prefer integers, so
e.g. `1E1` will be mapped to the integer `10`.

```Number -> integer() |
             float().
```

The special words will be mapped to atom().

```true -> true.
   false -> false.
   null -> undefined.
```
