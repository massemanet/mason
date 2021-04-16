Header "%% @hidden".

Nonterminals
  elements element value
  object array
  members member key.

Terminals
  ws
  '[' ']' '{' '}' ',' ':'
  float int bin string
  true false null.

Rootsymbol element.

element -> value        : '$1'.
element -> value ws     : '$1'.
element -> ws value     : '$2'.
element -> ws value ws  : '$2'.

value -> object   : '$1'.
value -> array    : '$1'.
value -> float    : val('$1').
value -> int      : val('$1').
value -> bin      : val('$1').
value -> string   : val('$1').
value -> true     : true.
value -> false    : false.
value -> null     : undefined.

object -> '{' '}'         : #{}.
object -> '{' ws '}'      : #{}.
object -> '{' members '}' : '$2'.

members -> member             : '$1'.
members -> members ',' member : maps:merge('$1', '$3').

member -> key ':' element : #{'$1' => '$3'}.

key -> string       : val('$1').
key -> string ws    : val('$1').
key -> ws string    : val('$2').
key -> ws string ws : val('$2').

array -> '[' ']'          : [].
array -> '[' ws ']'       : [].
array -> '[' elements ']' : '$2'.

elements -> element              : ['$1'].
elements -> elements ',' element : '$1' ++ ['$3'].

Erlang code.

val({_, _, E}) -> E.
