mason
=====

There are sooo many JSON libraries.

This one is geared towards logging. As such, it aims to handle some of the
conventional pseudo-types (such as string() and mfa()), and to decorate
e.g. pid() with meta info.

Mappings Erlang -> JSON.
===

Erlang has 11 types, the compiler-enforced pseudo-type record(), and a
number of pseudo-types that are purely by convention.

## Container types

There are three array-like types; one unordered/associative; `map()`,
and 2 ordered; `list()` and `tuple()`.

## Primitive types

The primitives are `number()` (with the subtypes `float()` and
`integer()`), the string identifier `atom()`, and the `bitstring()`
(which can hold any pattern of bits).

## Reference types

These reference various entities in the runtime. The `pid()` (a
process), the `port()` (similar to a file descriptor), the
`reference()` (on object whose only property is that it's guaranteed
to be unique), and the `function()` (a closure).

## Pseudo types

The `record()` looks like an assoc array in source, but at runtime it
is a tuple(). The compiler will perform transformations like
`R#rec.field` to `element(3, R)` using one mapping between field name
and tuple index per record definition (in this example something like
`{"rec" => {1 => "field"}}`). Note that this mapping is unique per
beam file; so record `rec` in file `f1` is not normally the same as
record `rec` in file `f2`. `mason` can import records definitions from
modules (more precisely, their beam files) with the option `{records
=> [module()]}`.

`ip()`
`ipp()`
`mfa()`
`datetime()`

## map()

A map() maps to an Object.

     map() -> Object.`

## list()

A list() naturally maps to an Array. There are (at least) two special
types of lists that are used as pseudo-types.

  * A string(), i.e. a list of characters, where a character is an
    integer in some range. The range can be Unicode code points,
    Latin-1, ASCII, etc.
      * `[printable()]` -> String.

  * A proplist(), an assoc list expressed as a list of 2-tuples.
    * `[{atom(), term()}]` -> Object.

A normal list is an ordered set of terms, which naturally maps to an
Array. Optionally, we can map to an Object with trivial keys.

  * A normal list.
    * `list()` -> Array.

With option `#{list => object}` encode list() as Object,
with the Object key being the position in the list.
    * E.g. `[1, a, "foo"] -> {"1": 1, "2": "a", "3": "foo"}`.


## tuple()

A tuple() naturally map to an Array. However, there are some special
tuples that are, by convention, used as pseudo-types.

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

A normal tuple is an ordered set of terms, which naturally maps to an
Array. Optionally, we can map to an Object with trivial keys.

  * A normal tuple.
    `tuple()` -> Array.

With option `#{tuple => object}` encode tuple() as
Object, with the Object key being the position in the tuple.

  * E.g. `[1, a, "foo"] -> {"1": 1, "2": "a", "3": "foo"}`.

## atom()

An atom() maps to a String. There are three exceptions; atoms that are
by convention used as booleans (`true` and `false`) and null
(`undefined`).

  * `true` -> true.
  * `false` -> false.
  * `undefined` -> null.
  * `atom()` -> String.

With option `#{atom => string}` all atoms are encoded as
String.

## number()

There are two kinds of numbers (integer() and float()). They map to
Number. However, an integer() can represent a timestamp, which should
be mapped to a String.

  * `timestamp()` -> String.
  * `float()` -> Number.
  * `integer()` -> Number.

With option `#{number => string}` all numbers are encoded as
String.

## bitstring()

A bitstring(), a.k.a. binary(), maps to a String. A binary is often
used to hold utf-8 encoded text, which trivially maps to
String. Otherwise, we encode as a hexstring (like "0x1f").

  * `bitstring()` -> String.

## pid()

A pid() is mapped to a String. We decorate with the name of the process, where
the name is either the registered name, or the initial call.

  * `pid()`-> String.

## reference()

A reference is mapped to a String.

  * `reference()` -> String.

## fun()

A function() is mapped to a String, using the MFA format.

  * `function()` -> String.

## port()

A port is mapped to a String. We decorate with the name of the port, and some
context dependent information about the port. E.g., if it's a socket, we add
the local IP:Port.

  * `port()` -> String.


Mappings JSON -> Erlang.
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
