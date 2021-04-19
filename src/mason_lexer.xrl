Definitions.

% separators
SEP = \[|\]|{|}|:|,

% whitespace
WS = [\n\r\t\s]+

% number
DIGIT = [0-9]
DIGITS = ({DIGIT}|[1-9]{DIGIT}+)
SIGN = [+-]
INT = {SIGN}?{DIGITS}
FLOAT = {INT}\.{DIGIT}+
EXP = [eE]{SIGN}?{DIGIT}+

% chars
HEX = [0-9A-Fa-f]
CHAR = [^"\\]
ESC = \\[bfnrt"/\\]
HEXESC = \\u{HEX}{HEX}{HEX}{HEX}
CHARS = ({CHAR}|{ESC}|{HEXESC})*

% reserved words
WORDS = true|false|null

Rules.

% number
{INT} :
  {token, {'int', TokenLine, list_to_integer(TokenChars)}}.

{FLOAT} :
  {token, {'float', TokenLine, list_to_float(TokenChars)}}.

{INT}{EXP} :
  {token, {'float', TokenLine, intexp_to_float(TokenChars)}}.

{FLOAT}{EXP} :
  {token, {'float', TokenLine, list_to_float(TokenChars)}}.

% binaries
"0x{HEX}*" :
  {token, {'bin', TokenLine, to_binary(TokenChars)}}.

% string
"{CHARS}" :
  {token, {'string', TokenLine, trim(TokenChars)}}.

% words
{WORDS} :
  {token, {list_to_atom(TokenChars), TokenLine}}.

% separators
{SEP} :
  {token, {list_to_atom(TokenChars), TokenLine}}.

% whitespace
{WS} :
  {token, {'ws', TokenLine, TokenChars}}.


Erlang code.

trim(Str) ->
    lists:reverse(tl(lists:reverse(tl(Str)))).

intexp_to_float(S) ->
    [A,B] = re:split(S, "[Ee]", [{return, list}]),
    list_to_float(lists:append([A,".0e", B])).

to_binary("\"0x"++R) ->
    to_binary(R, <<>>).

-define(upcase(C), $A =< C, C =< $F).
-define(lowcase(C), $a =< C, C =< $f).
-define(digit(C), $0 =< C, C =< $9).
to_binary("\"", O) ->
    O;
to_binary([C|R], O) when ?upcase(C) ->
    to_binary(R, <<O/bitstring, (C-$A+10):4>>);
to_binary([C|R], O) when ?lowcase(C) ->
    to_binary(R, <<O/bitstring, (C-$a+10):4>>);
to_binary([C|R], O) when ?digit(C) ->
    to_binary(R, <<O/bitstring, (C-$0):4>>).
