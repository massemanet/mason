%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(mason_encode).

-include_lib("eunit/include/eunit.hrl").

t_00_test() ->
    assert_eq('{}',
              #{}).
t_01_test() ->
    assert_eq('{"a":"b"}',
              #{a => b}).
t_02_test() ->
    assert_eq('{"a":"b"}',
              #{a => <<"b">>}).
t_03_test() ->
    assert_eq('{"a":"0x00"}',
              #{a => <<0:8>>}).
t_04_test() ->
    assert_eq('{"a":"0x6201"}',
              #{a => <<"b",1:8>>}).
t_05_test() ->
    assert_eq('{"a":"0x620"}',
              #{a => <<"b",0:4>>}).
t_06_test() ->
    assert_eq('{"a":1}',
              #{a => 1}).
t_07_test() ->
    assert_re('{"a":1\\.0+[eE]\\+0+}',
              #{a => 1.0}).
t_08_test() ->
    assert_re('"[a-z0-9_]+:[a-z0-9_-]+/[0-9]+-fun-[0-9]+-/[0-9]+"',
              fun(A)->A+1 end).
t_09_test() ->
    assert_re('"eunit_proc:run_test/1\\(<0\\.[0-9]+\\.[0-9]+>\\)"',
              self()).
t_10_test() ->
    assert_re('"#Ref<0\\.[0-9]+\\.[0-9]+\\.[0-9]+>"',
              make_ref()).
t_11_test() ->
    assert_re('\\[.*\\]',
              erlang:ports()).
t_12_test() ->
    assert_re('\\["a","b","c"\\]',
              [a, b, c]).
t_13_test() ->
    assert_re('\\["a","b","c"\\]',
              {a, b, c}).
t_14_test() ->
    assert_re('\\[\\]',
              []).
t_15_test() ->
    assert_re('\\[\\]',
              {}).
t_16_test() ->
    assert_eq('"0x0123456789abcdef"',
              <<0:4,1:4,2:4,3:4,4:4,5:4,6:4,7:4,8:4,9:4,10:4,11:4,12:4,13:4,14:4,15:4>>).
t_17_test() ->
    assert_eq('"127.0.0.1:666"',
              {{127,0,0,1}, 666}).
t_18_test() ->
    assert_eq('[["case_clause",["listening","closed"]],["pamp_sctp:check_state/4:254","pamp_psock:socket_read/1:457"]]',
              {{case_clause,{listening,closed}},
               [{pamp_sctp,check_state,4,
                 [{file,"/build/apps/pamp/src/pamp_sctp.erl"}, {line,254}]},
                {pamp_psock,socket_read,1,
                 [{file,"/build/apps/pamp/src/pamp_psock.erl"}, {line,457}]}]}).

t_19_test() ->
    assert_eq('[["a","b"],["c"]]',
              {{a, b}, [c]}).
t_20_test() ->
    assert_eq('[33,44,0]',
              [33,44,0]).
t_21_test() ->
    assert_eq('"!,"',
              [33,44]).
t_22_test() ->
    assert_eq('{"a":"b","c":"d"}',
              {{a, b}, {c, d}}).
t_23_test() ->
    assert_eq('{"a":"b","c":"d"}',
              [{a, b}, {c, d}]).
t_24_test() ->
    assert_eq('[["a","b"],[1,"d"]]',
              [{a, b}, {1, d}]).

t_25_test() ->
    assert_eq('{"a":["a","b"]}',
              [a | {a, b}]).

t_26_test() ->
    assert_eq('[49,["a","b"]]',
              [49 | {a, b}]).

t_27_test() ->
    assert_eq('["a",49,[1,"d"]]',
              [a , 49 | {1, d}]).

t_28_test() ->
    assert_eq('[49,"a",["a","d"]]',
              [49, a | {a, d}]).

assert_eq(Goal, In) ->
    ?assertEqual(atom_to_list(Goal), mason:encode(In)).

assert_re(RE, In) ->
    {ok, CRE} = re:compile(atom_to_list(RE)),
    Enc = mason:encode(In),
    L = length(Enc),
    case re:run(Enc, CRE) of
        {match, [{0, L}]} -> ok;
        _ -> ?assertEqual(RE, Enc)
    end.
