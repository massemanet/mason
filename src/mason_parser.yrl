Header "%% @hidden".

Nonterminals
  elements element value
  object array
  members member key.

Terminals
  ws
  '[' ']' '{' '}' ',' ':'
  number string
  true false null.

Rootsymbol element.

element -> value        : '$1'.
element -> value ws     : '$1'.
element -> ws value     : '$2'.
element -> ws value ws  : '$2'.

value -> object   : '$1'.
value -> array    : '$1'.
value -> number   : val('$1').
value -> string   : val('$1').
value -> true     : true.
value -> false    : false.
value -> null     : undefined.

object -> '{' '}'         : object(undefined, undefined).
object -> '{' ws '}'      : object(undefined, undefined).
object -> '{' members '}' : '$2'.

members -> member             : object(undefined, '$1').
members -> members ',' member : object('$1', '$3').

member -> key ':' element : #{'$1' => '$3'}.

key -> string       : key('$1').
key -> string ws    : key('$1').
key -> ws string    : key('$2').
key -> ws string ws : key('$2').

array -> '[' ']'          : array(undefined, undefined).
array -> '[' ws ']'       : array(undefined, undefined).
array -> '[' elements ']' : '$2'.

elements -> element              : array(undefined, '$1').
elements -> elements ',' element : array('$1', '$3').

Erlang code.

val({Class, _, Val}) -> mason_decoder:go(val, Class, Val).
key({Class, _, Val}) -> mason_decoder:go(key, Class, Val).
array(Array, Element) -> mason_decoder:go(array, Array, Element).
object(Object, Member) -> mason_decoder:go(object, Object, Member).
