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
TAILBITS = (:[1-3])

% time
YEAR = [0-9][0-9][0-9][0-9]
MONTH = (0[1-9]|1[0-2])
DAY = (0[1-9]|[12][0-9]|3[0-1])
HOUR = ([01][0-9]|2[0-4])
MIN = [0-5][0-9]
SEC = ([0-5][0-9]|60)
FRAC = (\.[0-9]+)
TZ = (Z|[+-]{HOUR}:{MIN})

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

% verbose timestring, similar to RFC 3339
"{YEAR}-{MONTH}-{DAY}T{HOUR}:{MIN}:{SEC}{FRAC}?{TZ}?" :
  {token, {'string', TokenLine, {time, trim(TokenChars)}}}.

% compact timestring, similar to RFC 3339
"{YEAR}{MONTH}{DAY}T{HOUR}{MIN}{SEC}{FRAC}?{TZ}?" :
  {token, {'string', TokenLine, {time, trim(TokenChars)}}}.

% hexstring
"0x{HEX}+{TAILBITS}?" :
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
