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
value -> true     : val('$1').
value -> false    : val('$1').
value -> null     : val('$1').

object -> '{' '}'         : object(nil, nil).
object -> '{' ws '}'      : object(nil, nil).
object -> '{' members '}' : '$2'.

members -> member             : object(nil, '$1').
members -> members ',' member : object('$1', '$3').

member -> key ':' element : {'$1', '$3'}.

key -> string       : key('$1').
key -> string ws    : key('$1').
key -> ws string    : key('$2').
key -> ws string ws : key('$2').

array -> '[' ']'          : array(nil, nil).
array -> '[' ws ']'       : array(nil, nil).
array -> '[' elements ']' : '$2'.

elements -> element              : array(nil, '$1').
elements -> elements ',' element : array('$1', '$3').

Erlang code.

val({Val, _}) -> mason_decoder:go(val, word, Val);
val({Class, _, Val}) -> mason_decoder:go(val, Class, Val).
key({Class, _, Val}) -> mason_decoder:go(key, Class, Val).
array(Array, Element) -> mason_decoder:go(array, Array, Element).
object(Object, Member) -> mason_decoder:go(object, Object, Member).
