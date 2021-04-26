mason
=====

There are sooo many JSON libraries.

Mappings erlang -> JSON.
===

map() -> Object.

list({atom(), term()}) -> Object.
list(printable()) -> String.
lists() -> Array.

{atom(), atom(), integer()} -> String.
{byte(), byte(), byte(), byte()} -> String.
{{byte(), byte(), byte(), byte()}, long()} -> String.
{atom(), atom(), integer(), [_, {line, integer()}]} -> String.
tuple() -> Array.

true -> true.
false -> false.
undefined -> null.

float() -> Number.
integer() -> Number.
binary() -> String.
atom() -> String
pid() -> String.
reference() -> String.
function() -> String.
port() -> String.

Mappings JSON -> erlang.
===

Object -> map(atom(), term()) | map(string(), term()) | map(binary(), term()) | list(atom(), term()).
Array -> tuple() | list().
String -> string() | binary() | atom().
Number -> integer() | float().
true -> true.
false -> false.
null -> undefined.
