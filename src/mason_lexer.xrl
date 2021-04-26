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
  {token, {'number', TokenLine, {int, TokenChars}}}.

{FLOAT} :
  {token, {'number', TokenLine, {float, TokenChars}}}.

{INT}{EXP} :
  {token, {'number', TokenLine, {intexp, TokenChars}}}.

{FLOAT}{EXP} :
  {token, {'number', TokenLine, {floatexp, TokenChars}}}.

% binaries
"0x{HEX}*" :
  {token, {'string', TokenLine, {hex, trim(TokenChars)}}}.

% string
"{CHARS}" :
  {token, {'string', TokenLine, {chars, trim(TokenChars)}}}.

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
