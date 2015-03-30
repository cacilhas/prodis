% -*- Prolog -*-
% @copyright ©2015, Rodrigo Cacilhας <batalema@cacilhas.info>
:- module(redis_server, [redis_process_command/3]).
:- [library(settings), logging, database, autosave, operators].

redis_process_command(Command, Parameters, Response) :-
	process(Command, Parameters, Response).


%% QUIT ------------------------------------------------------------------------
process('QUIT', [], '+CLOSED\r~n') :- !.


%% APPEND ----------------------------------------------------------------------
process('APPEND', [Key, _], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
	regget(Key, Value),
	(member(type-hash, Value); member(type-list, Value)), !.

process('APPEND', [Key, Value], R) :-
	regget(Key, [type-string, key-Key, value-Current]), !,
    term_string(Value, Value1),
    p_append(Value1, Current, Value2),
	regset(Key, Value2),
    string_length(Value2, Length),
    term_output(Length, R).

process('APPEND', [Key, Value], R) :-
    \+ regget(Key, _), !,
    term_string(Value, Value1),
	regset(Key, Value1),
    string_length(Value1, Length),
    term_output(Length, R).


%% AUTH ------------------------------------------------------------------------
process('AUTH', [Auth], '+OK\r~n') :-
    setting(prodis:password, ValidPasswords),
    member(Auth, ValidPasswords), !.

process('AUTH', [_], '-ERR invalid password\r~n') :- !.


%% BGREWRITEAOF ----------------------------------------------------------------
process('BGREWRITEAOF', _, '-ERR not implemented\r~n') :- !.


%% BGSAVE ----------------------------------------------------------------------
process('BGSAVE', [], '+Background saving started\r~n') :- !,
    thread_create(autosave:save, _, [detached(true)]).


%% BITCOUNT --------------------------------------------------------------------
process('BITCOUNT', [Key], R) :-
	regget(Key, [type-Type, key-Key, value-Value]),
	member(Type, [string, number]),
    (string(Value) -> string_codes(Value, Value1); Value1 = Value),
    count_bits(Value1, Bits), !,
    term_output(Bits, R).

process('BITCOUNT', [_], ':0\r~n') :- !.


%% BITOP -----------------------------------------------------------------------
process('BITOP', [Op, Dest | Params], R) :-
    string_upper(Op, Aux),
    atom_string(Operator, Aux),
    bitop(Operator, Dest, Params, R), !.

process('BITOP', _, '-ERR wrong parameters to BITOP\r~n').


%% BITPOS ----------------------------------------------------------------------
process('BITPOS', _, '-ERR not implemented\r~n') :- !.


%% BLPOP -----------------------------------------------------------------------
process('BLPOP', Param, R) :- !,
    process('LPOP', Param, R).


%% BRPOP -----------------------------------------------------------------------
process('BRPOP', Param, R) :- !,
    process('RPOP', Param, R).


%% BRPOPLPUSH ------------------------------------------------------------------
process('BRPOPLPUSH', Param, R) :- !,
    process('RPOPLPUSH', Param, R).


%% CLIENT KILL -----------------------------------------------------------------
process('CLIENT', [Op | Params], R) :-
    string_upper(Op, Aux),
    atom_string(Operator, Aux),
    client(Operator, Params, R), !.

process('CLIENT', _, '-ERR wrong parameters to CLIENT\r~n').


%% CLUSTER ---------------------------------------------------------------------
process('CLUSTER', _, '-ERR not implemented\r~n') :- !.


%% COMMAND ---------------------------------------------------------------------
process('COMMAND', [], '-ERR not implemented\r~n') :- !.


%% COMMAND COUNT ---------------------------------------------------------------
process('COMMAND', [Op | Params], R) :-
    string_upper(Op, Aux),
    atom_string(Operator, Aux),
    command(Operator, Params, R), !.

process('COMMAND', _, '-ERR wrong parameters to COMMAND\r~n').


%% CONFIG ----------------------------------------------------------------------
process('CONFIG', [Op | Params], R) :-
    string_upper(Op, Aux),
    atom_string(Operator, Aux),
    config(Operator, Params, R), !.

process('CONFIG', _, '-ERR wrong parameters to CONFIG\r~n').


%% DBSIZE ----------------------------------------------------------------------
process('DBSIZE', [], R) :-
	regkeys("*", Keys),
	length(Keys, Length),
    term_output(Length, R).


%% DEBUG OBJECT ----------------------------------------------------------------
process('DEBUG', _, '-ERR not implemented\r~n') :- !.


%% DECR ------------------------------------------------------------------------
process('DECR', [Key], R) :-
    process('DECRBY', [Key, 1], R).


%% DECRBY ----------------------------------------------------------------------
process('DECRBY', [Key, Dec], R) :-
    regget(Key, [type-number, key-Key, value-Value]), !,
    V1 is Value - Dec,
    regset(Key, V1),
    term_output(V1, R).

process('DECRBY', [Key, _], '-ERR value is not an integer or out of range') :-
	regget(Key, _), !.

process('DECRBY', [Key, Dec], R) :- !,
    Value is -Dec,
	regset(Key, Value),
    term_output(Value, R).


%% DEL -------------------------------------------------------------------------
process('DEL', [Key], ':1\r~n') :-
	regexists(Key), !,
	regdel(Key).

process('DEL', [_], ':0\r~n') :- !.


%% DISCARD ---------------------------------------------------------------------
process('DISCARD', _, '-ERR not implemented\r~n') :- !.


%% DUMP ------------------------------------------------------------------------
process('DUMP', [Key], R) :-
    regget(Key, [_, key-Key, value-Value]), !,
    term_output(Value, R).

process('DUMP', [_], '$-1\r~n') :- !.


%% ECHO ------------------------------------------------------------------------
process('ECHO', [String], R) :- !,
    term_output(String, R).


%% EVAL ------------------------------------------------------------------------
process('EVAL', _, '-ERR not implemented\r~n') :- !.


%% EVALSHA ---------------------------------------------------------------------
process('EVALSHA', _, '-ERR not implemented\r~n') :- !.


%% EXEC ------------------------------------------------------------------------
process('EXEC', _, '-ERR not implemented\r~n') :- !.


%% EXISTS ----------------------------------------------------------------------
process('EXISTS', [Key], ':1\r~n') :-
    regexists(Key), !.

process('EXISTS', [_], ':0\r~n') :- !.


%% EXPIRE ----------------------------------------------------------------------
process('EXPIRE', _, '-ERR not implemented\r~n') :- !.


%% EXPIREAT --------------------------------------------------------------------
process('EXPIREAT', _, '-ERR not implemented\r~n') :- !.


%% FLUSHALL --------------------------------------------------------------------
process('FLUSHALL', [], '+OK\r~n') :-
	regdel(_).


%% FLUSHDB ---------------------------------------------------------------------
process('FLUSHDB', [], '+OK\r~n') :-
	regdel(_).


%% GET -------------------------------------------------------------------------
process('GET', [Key], R) :-
	regexists(Key),
    regget(Key, [type-Type, key-Key, value-Value]),
	member(Type, [string, number]), !,
    term_output(Value, R).

process('GET', [Key], '$-1\r~n') :-
	\+ regexists(Key), !.


%% GETBIT ----------------------------------------------------------------------
process('GETBIT', _, '-ERR not implemented\r~n') :- !.


%% GETRANGE --------------------------------------------------------------------
process('GETRANGE', _, '-ERR not implemented\r~n') :- !.


%% GETSET ----------------------------------------------------------------------
process('GETSET', [Key, Value], R) :-
    process('GET', [Key], R),
    process('SET', [Key, Value], _).


%%
%% On SWI 7, change Field(Value) by Field:Value
%%

%% HDEL ------------------------------------------------------------------------

process('HDEL', [Key, Field], ':1\r~n') :-
	regexists(Key, Field), !,
	regdel(Key, Field).

process('HDEL', [Key, _], ':0\r~n') :-
    regget(Key, [type-hash |_]), !.

process('HDEL', [Key, _], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
    regget(Key, Value), Value \= null, !.

process('HDEL', [_, _], ':0\r~n') :- !.


%% HEXISTS ---------------------------------------------------------------------
process('HEXISTS', [Key, Field], ':1\r~n') :-
	regexists(Key, Field), !.

process('HEXISTS', [Key, _], ':0\r~n') :-
	regget(Key, [type-hash |_]), !.

process('HEXISTS', [Key, _], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
    regget(Key, Value), Value \= null, !.

process('HEXISTS', [_, _], ':0\r~n') :- !.


%% HGET ------------------------------------------------------------------------
process('HGET', [Key, Field], R) :-
	regget(Key, Field, [_, key-Key, field-Field, value-Value]), !,
    term_output(Value, R).

process('HGET', [Key, _], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
    regget(Key, Value), Value \= null, !.

process('HGET', [_, _], '$-1\r~n') :- !.


%% HGETALL ---------------------------------------------------------------------
process('HGETALL', [Key], R) :-
	regget(Key, [type-hash |_]), !,
    findall(X, (regget(Key, [type-hash, key-Key, value-Value]), X =.. Value), List),
	flatten(List, List1),
    term_output(List1, R).

process('HGETALL', [Key], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
    regget(Key, Value), Value \= null, !.

process('HGETALL', [_], '*0\r~n') :- !.


%% HINCRBY ---------------------------------------------------------------------
process('HINCRBY', [Key, Field, Increment], R) :-
	regget(Key, Field, [type-number, key-Key, field-Field, value-Value]), !,
	Value1 is Value + Increment,
	Hash1 =.. [Field, Value1],
	regset(Key, Hash1),
	term_output(Value1, R).

%% HINCRBYFLOAT ----------------------------------------------------------------
process('HINCRBYFLOAT', _, '-ERR not implemented\r~n') :- !.


%% HKEYS -----------------------------------------------------------------------
process('HKEYS', [Key], R) :-
	regfields(Key, Fields),
	term_output(Fields, R).


%% HLEN ------------------------------------------------------------------------
process('HLEN', [Key], R) :-
	regfields(Key, Fields),
	length(Fields, Length),
	term_output(Length, R).


%% HMGET -----------------------------------------------------------------------
process('HMGET', _, '-ERR not implemented\r~n') :- !.


%% HMSET -----------------------------------------------------------------------
process('HMSET', _, '-ERR not implemented\r~n') :- !.


%% HSET ------------------------------------------------------------------------
process('HSET', [Key, Field, Value], R) :-
	regget(Key, [type-hash |_]), !,
	Hash =.. [Field, Value],
	regset(Key, Hash),
	regfields(Key, Fields),
	length(Fields, Length),
	term_output(Length, R).

process('HSET', [Key, _, _], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
	regexists(Key), !.

process('HSET', [_, _, _], ':0\r~n') :- !.


%% HSETNX ----------------------------------------------------------------------
process('HSETNX', _, '-ERR not implemented\r~n') :- !.


%% HSTRLEN ---------------------------------------------------------------------
process('HSTRLEN', _, '-ERR not implemented\r~n') :- !.


%% HVALS -----------------------------------------------------------------------
process('HVALS', _, '-ERR not implemented\r~n') :- !.


%% INCR ------------------------------------------------------------------------
process('INCR', [Key], R) :-
	process('INCRBY', [Key, 1], R).


%% INCRBY ----------------------------------------------------------------------
process('INCRBY', [Key, Increment], R) :-
	regget(Key, [type-number, key-Key, value-Value]), !,
	Value1 is Value + Increment,
	regset(Key, Value1),
	term_output(Value1, R).


%% INCRBYFLOAT -----------------------------------------------------------------
process('INCRBYFLOAT', _, '-ERR not implemented\r~n') :- !.


%% INFO ------------------------------------------------------------------------
process('INFO', _, '-ERR not implemented\r~n') :- !.


%% KEYS ------------------------------------------------------------------------
process('KEYS', [Pattern], R) :-
	regkeys(Pattern, Keys),
	term_output(Keys, R).


%% LASTSAVE --------------------------------------------------------------------
process('LASTSAVE', _, '-ERR not implemented\r~n') :- !.


%% LINDEX ----------------------------------------------------------------------
process('LINDEX', [Key, Index], R) :-
	regget(Key, [type-list, key-Key, value-List]), !,
	(nth0(Index, List, Value); Value = null),
	term_output(Value, R).

process('LINDEX', [Key, _], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
	regexists(Key), !.

process('LINDEX', [_, _], '$-1\r~n') :- !.


%% LINSERT ---------------------------------------------------------------------
process('LINSERT', _, '-ERR not implemented\r~n') :- !.


%% LLEN ------------------------------------------------------------------------
process('LLEN', [Key], R) :-
	regget(Key, [type-list, key-Key, value-List]), !,
	length(List, Length),
	term_output(Length, R).

process('LLEN', [Key], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
    regexists(Key), !.

process('LLEN', [_], ':0\r~n') :- !.


%% LPOP ------------------------------------------------------------------------
process('LPOP', [Key], R) :-
	regget(Key, [type-list, key-Key, value-[Left]]), !,
	regdel(Key),
	term_output(Left, R).

process('LPOP', [Key], R) :-
	regget(Key, [type-list, key-Key, value-[Left|List]]), !,
	regset(Key, List),
	term_output(Left, R).

process('LPOP', [Key], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
    regexists(Key), !.

process('LPOP', [_], '$-1\r~n') :- !.



%% LPUSH -----------------------------------------------------------------------
process('LPUSH', [Key|Values], R) :-
	regget(Key, [type-list, key-Key, value-List]), !,
	append(Values, List, List1),
	regset(Key, List1),
	length(List1, Length),
	term_output(Length, R).

process('LPUSH', [Key], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
    regexists(Key), !.

process('LPUSH', [Key|Values], R) :-
	regset(Key, Values),
	length(Values, Length),
	term_output(Length, R).



%% LPUSHX ----------------------------------------------------------------------
process('LPUSHX', [Key, Value], R) :-
	regexists(Key), !,
	process('LPUSH', [Key, Value], R).

process('LPUSHX', [Key, _], ':0\r~n') :-
	\+ regexists(Key), !.


%% LRANGE ----------------------------------------------------------------------
process('LRANGE', _, '-ERR not implemented\r~n') :- !.


%% LREM ------------------------------------------------------------------------
process('LREM', _, '-ERR not implemented\r~n') :- !.


%% LSET ------------------------------------------------------------------------
process('LSET', _, '-ERR not implemented\r~n') :- !.


%% LTRIM -----------------------------------------------------------------------
process('LTRIM', _, '-ERR not implemented\r~n') :- !.


%% MGET ------------------------------------------------------------------------
process('MGET', _, '-ERR not implemented\r~n') :- !.


%% MIGRATE ---------------------------------------------------------------------
process('MIGRATE', _, '-ERR not implemented\r~n') :- !.


%% MONITOR ---------------------------------------------------------------------
process('MONITOR', _, '-ERR not implemented\r~n') :- !.


%% MOVE ------------------------------------------------------------------------
process('MOVE', _, '-ERR not implemented\r~n') :- !.


%% MSET ------------------------------------------------------------------------
process('MSET', _, '-ERR not implemented\r~n') :- !.


%% MSETNX ----------------------------------------------------------------------
process('MSETNX', _, '-ERR not implemented\r~n') :- !.


%% MULTI -----------------------------------------------------------------------
process('MULTI', _, '-ERR not implemented\r~n') :- !.


%% OBJECT ----------------------------------------------------------------------
process('OBJECT', _, '-ERR not implemented\r~n') :- !.


%% PERSIST ---------------------------------------------------------------------
process('PERSIST', _, '-ERR not implemented\r~n') :- !.


%% PEXPIRE ---------------------------------------------------------------------
process('PEXPIRE', _, '-ERR not implemented\r~n') :- !.


%% PEXPIREAT -------------------------------------------------------------------
process('PEXPIREAT', _, '-ERR not implemented\r~n') :- !.


%% PFADD -----------------------------------------------------------------------
process('PFADD', _, '-ERR not implemented\r~n') :- !.


%% PFCOUNT ---------------------------------------------------------------------
process('PFCOUNT', _, '-ERR not implemented\r~n') :- !.


%% PFMERGE ---------------------------------------------------------------------
process('PFMERGE', _, '-ERR not implemented\r~n') :- !.


%% PING ------------------------------------------------------------------------
process('PING', [], '+PONG\r~n') :- !.


%% PSETEX ----------------------------------------------------------------------
process('PSETEX', _, '-ERR not implemented\r~n') :- !.


%% PSUBSCRIBE ------------------------------------------------------------------
process('PSUBSCRIBE', _, '-ERR not implemented\r~n') :- !.


%% PUBSUB ----------------------------------------------------------------------
process('PUBSUB', _, '-ERR not implemented\r~n') :- !.


%% PTTL ------------------------------------------------------------------------
process('PTTL', _, '-ERR not implemented\r~n') :- !.


%% PUBLISH ---------------------------------------------------------------------
process('PUBLISH', _, '-ERR not implemented\r~n') :- !.


%% PUNSUBSCRIBE ----------------------------------------------------------------
process('PUNSUBSCRIBE', _, '-ERR not implemented\r~n') :- !.


%% RANDOMKEY -------------------------------------------------------------------
process('RANDOMKEY', _, '-ERR not implemented\r~n') :- !.


%% RENAME ----------------------------------------------------------------------
process('RENAME', _, '-ERR not implemented\r~n') :- !.


%% RENAMENX --------------------------------------------------------------------
process('RENAMENX', _, '-ERR not implemented\r~n') :- !.


%% RESTORE ---------------------------------------------------------------------
process('RESTORE', _, '-ERR not implemented\r~n') :- !.


%% ROLE ------------------------------------------------------------------------
process('ROLE', _, '-ERR not implemented\r~n') :- !.


%% RPOP ------------------------------------------------------------------------
process('RPOP', [Key], R) :-
	regget(Key, [type-list, key-Key, value-[Right]]), !,
	regdel(Key),
	term_output(Right, R).

process('RPOP', [Key], R) :-
	regget(Key, [type-list, key-Key, value-List]), !,
	append(List1, [Right], List), !,
	regset(Key, List1),
	term_output(Right, R).

process('RPOP', [Key], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
    regexists(Key), !.

process('RPOP', [_], '$-1\r~n') :- !.


%% RPOPLPUSH -------------------------------------------------------------------
process('RPOPLPUSH', [_, Destination], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
	regget(Destination, [type-Type |_]),
	Type \= list, !.

process('RPOPLPUSH', [Source, Destination], R) :-
	regget(Source, [type-list, key-Source, value-[Right]]), !,
	regdel(Source),
	term_output(Right, R),
	process('LPUSH', [Destination, Right], _).

process('RPOPLPUSH', [Source, Destination], R) :-
	regget(Source, [type-list, key-Source, value-List]),
	append(List1, [Right], List), !,
	regset(Source, List1),
	term_output(Right, R),
	process('LPUSH', [Destination, Right], _).

process('RPOPLPUSH', [Source, _], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
    regexists(Source), !.

process('RPOPLPUSH', [_, _], '$-1\r~n') :- !.


%% RPUSH -----------------------------------------------------------------------
process('RPUSH', [Key|Values], R) :-
	regget(Key, [type-list, key-Key, value-List]), !,
	append(List, Values, List1),
	regset(Key, List1),
	length(List1, Length),
	term_output(Length, R).

process('RPUSH', [Key], '-WRONGTYPE Operation against a key holding the wrong kind of value') :-
    regexists(Key), !.

process('RPUSH', [Key|Values], R) :- !,
	regset(Key, Values),
	length(Values, Length),
	term_output(Length, R).


%% RPUSHX ----------------------------------------------------------------------
process('RPUSHX', [Key, Value], R) :-
	regexists(Key), !,
	process('RPUSH', [Key, Value], R).

process('RPUSHX', [Key, _], ':0\r~n') :-
	\+ regexists(Key), !.


%% SADD ------------------------------------------------------------------------
process('SADD', _, '-ERR not implemented\r~n') :- !.


%% SAVE ------------------------------------------------------------------------
process('SAVE', [], '+OK\r~n') :-
    autosave:save, !.

process('SAVE', [], '-ERR\r~n') :- !.


%% SCARD -----------------------------------------------------------------------
process('SCARD', _, '-ERR not implemented\r~n') :- !.


%% SCRIPT EXISTS ---------------------------------------------------------------
process('SCRIPT', _, '-ERR not implemented\r~n') :- !.


%% SCRIPT FLUSH ----------------------------------------------------------------
process('SCRIPT', _, '-ERR not implemented\r~n') :- !.


%% SCRIPT KILL -----------------------------------------------------------------
process('SCRIPT', _, '-ERR not implemented\r~n') :- !.


%% SCRIPT LOAD -----------------------------------------------------------------
process('SCRIPT', _, '-ERR not implemented\r~n') :- !.


%% SDIFF -----------------------------------------------------------------------
process('SDIFF', _, '-ERR not implemented\r~n') :- !.


%% SDIFFSTORE ------------------------------------------------------------------
process('SDIFFSTORE', _, '-ERR not implemented\r~n') :- !.


%% SELECT ----------------------------------------------------------------------
process('SELECT', _, '-ERR not implemented\r~n') :- !.


%% SET -------------------------------------------------------------------------
process('SET', [Key, Value], ':1\r~n') :- !,
	regset(Key, Value).


%% SETBIT ----------------------------------------------------------------------
process('SETBIT', _, '-ERR not implemented\r~n') :- !.


%% SETEX -----------------------------------------------------------------------
process('SETEX', _, '-ERR not implemented\r~n') :- !.


%% SETNX -----------------------------------------------------------------------
process('SETNX', _, '-ERR not implemented\r~n') :- !.


%% SETRANGE --------------------------------------------------------------------
process('SETRANGE', _, '-ERR not implemented\r~n') :- !.


%% SHUTDOWN --------------------------------------------------------------------
process('SHUTDOWN', [How], '+OK\r~n') :-
    string_upper(How, Aux),
	atom_string(How1, Aux),
	member(How1, ['SAVE', 'NOSAVE']), !,
	(How1 = 'SAVE' -> autosave:save; true),
	halt.


%% SINTER ----------------------------------------------------------------------
process('SINTER', _, '-ERR not implemented\r~n') :- !.


%% SINTERSTORE -----------------------------------------------------------------
process('SINTERSTORE', _, '-ERR not implemented\r~n') :- !.


%% SISMEMBER -------------------------------------------------------------------
process('SISMEMBER', _, '-ERR not implemented\r~n') :- !.


%% SLAVEOF ---------------------------------------------------------------------
process('SLAVEOF', _, '-ERR not implemented\r~n') :- !.


%% SLOWLOG ---------------------------------------------------------------------
process('SLOWLOG', _, '-ERR not implemented\r~n') :- !.


%% SMEMBERS --------------------------------------------------------------------
process('SMEMBERS', _, '-ERR not implemented\r~n') :- !.


%% SMOVE -----------------------------------------------------------------------
process('SMOVE', _, '-ERR not implemented\r~n') :- !.


%% SORT ------------------------------------------------------------------------
process('SORT', _, '-ERR not implemented\r~n') :- !.


%% SPOP ------------------------------------------------------------------------
process('SPOP', _, '-ERR not implemented\r~n') :- !.


%% SRANDMEMBER -----------------------------------------------------------------
process('SRANDMEMBER', _, '-ERR not implemented\r~n') :- !.


%% SREM ------------------------------------------------------------------------
process('SREM', _, '-ERR not implemented\r~n') :- !.


%% STRLEN ----------------------------------------------------------------------
process('STRLEN', [Key], R) :-
	regget(Key, [type-string, key-Key, value-Value]), !,
	string_length(Value, Length),
	term_output(Length, R).


%% SUBSCRIBE -------------------------------------------------------------------
process('SUBSCRIBE', _, '-ERR not implemented\r~n') :- !.


%% SUNION ----------------------------------------------------------------------
process('SUNION', _, '-ERR not implemented\r~n') :- !.


%% SUNIONSTORE -----------------------------------------------------------------
process('SUNIONSTORE', _, '-ERR not implemented\r~n') :- !.


%% SYNC ------------------------------------------------------------------------
process('SYNC', _, '-ERR not implemented\r~n') :- !.


%% TIME ------------------------------------------------------------------------
process('TIME', [], R) :- !,
	get_time(Stamp),
	format_time(atom(R), '%YT%T%z', Stamp).


%% TTL -------------------------------------------------------------------------
process('TTL', _, '-ERR not implemented\r~n') :- !.


%% TYPE ------------------------------------------------------------------------
process('TYPE', [Key], R) :-
	regget(Key, [type-Type |_]), !,
	term_output(Type, R).

process('TYPE', [_], '+none\r~n') :- !.


%% UNSUBSCRIBE -----------------------------------------------------------------
process('UNSUBSCRIBE', _, '-ERR not implemented\r~n') :- !.


%% UNWATCH ---------------------------------------------------------------------
process('UNWATCH', _, '-ERR not implemented\r~n') :- !.


%% WATCH -----------------------------------------------------------------------
process('WATCH', _, '-ERR not implemented\r~n') :- !.


%% ZADD ------------------------------------------------------------------------
process('ZADD', _, '-ERR not implemented\r~n') :- !.


%% ZCARD -----------------------------------------------------------------------
process('ZCARD', _, '-ERR not implemented\r~n') :- !.


%% ZCOUNT ----------------------------------------------------------------------
process('ZCOUNT', _, '-ERR not implemented\r~n') :- !.


%% ZINCRBY ---------------------------------------------------------------------
process('ZINCRBY', _, '-ERR not implemented\r~n') :- !.


%% ZINTERSTORE -----------------------------------------------------------------
process('ZINTERSTORE', _, '-ERR not implemented\r~n') :- !.


%% ZLEXCOUNT -------------------------------------------------------------------
process('ZLEXCOUNT', _, '-ERR not implemented\r~n') :- !.


%% ZRANGE ----------------------------------------------------------------------
process('ZRANGE', _, '-ERR not implemented\r~n') :- !.


%% ZRANGEBYLEX -----------------------------------------------------------------
process('ZRANGEBYLEX', _, '-ERR not implemented\r~n') :- !.


%% ZREVRANGEBYLEX --------------------------------------------------------------
process('ZREVRANGEBYLEX', _, '-ERR not implemented\r~n') :- !.


%% ZRANGEBYSCORE ---------------------------------------------------------------
process('ZRANGEBYSCORE', _, '-ERR not implemented\r~n') :- !.


%% ZRANK -----------------------------------------------------------------------
process('ZRANK', _, '-ERR not implemented\r~n') :- !.


%% ZREM ------------------------------------------------------------------------
process('ZREM', _, '-ERR not implemented\r~n') :- !.


%% ZREMRANGEBYLEX --------------------------------------------------------------
process('ZREMRANGEBYLEX', _, '-ERR not implemented\r~n') :- !.


%% ZREMRANGEBYRANK -------------------------------------------------------------
process('ZREMRANGEBYRANK', _, '-ERR not implemented\r~n') :- !.


%% ZREMRANGEBYSCORE ------------------------------------------------------------
process('ZREMRANGEBYSCORE', _, '-ERR not implemented\r~n') :- !.


%% ZREVRANGE -------------------------------------------------------------------
process('ZREVRANGE', _, '-ERR not implemented\r~n') :- !.


%% ZREVRANGEBYSCORE ------------------------------------------------------------
process('ZREVRANGEBYSCORE', _, '-ERR not implemented\r~n') :- !.


%% ZREVRANK --------------------------------------------------------------------
process('ZREVRANK', _, '-ERR not implemented\r~n') :- !.


%% ZSCORE ----------------------------------------------------------------------
process('ZSCORE', _, '-ERR not implemented\r~n') :- !.


%% ZUNIONSTORE -----------------------------------------------------------------
process('ZUNIONSTORE', _, '-ERR not implemented\r~n') :- !.


%% SCAN ------------------------------------------------------------------------
process('SCAN', _, '-ERR not implemented\r~n') :- !.


%% SSCAN -----------------------------------------------------------------------
process('SSCAN', _, '-ERR not implemented\r~n') :- !.


%% HSCAN -----------------------------------------------------------------------
process('HSCAN', _, '-ERR not implemented\r~n') :- !.


%% ZSCAN -----------------------------------------------------------------------
process('ZSCAN', _, '-ERR not implemented\r~n') :- !.


%% Unknown command -------------------------------------------------------------
process(Command, _, R) :-
	format(atom(R), '-ERR unknown command or parameters: ~w\r~n', [Command]).


%% Auxiliar --------------------------------------------------------------------
bitop(Op, Dest, Params, R) :-
    findall(X, (member(Key, Params), regget(Key, [type-string, key-Key, value-X])), Values),
    do_bitop(Op, Values, [], Value),
	regset(Dest, Value),
    length(Value, Length),
    term_output(Length, R).

do_bitop(_, [], Value, Value).

do_bitop('NOT', [Value], _, R) :-
    apply_not(Value, R).

do_bitop('AND', [X|Xs], Acc, R) :-
    apply_and(X, Acc, Acc1),
    do_bitop('AND', Xs, Acc1, R).

do_bitop('OR', [X|Xs], Acc, R) :-
    apply_or(X, Acc, Acc1),
    do_bitop('OR', Xs, Acc1, R).

do_bitop('XOR', [X|Xs], Acc, Value) :-
    apply_xor(X, Acc, Acc1),
    do_bitop('XOR', Xs, Acc1, Value).


client('GETNAME', _, '-ERR not implemented\r~n').

client('KILL', _, '-ERR not implemented\r~n').

client('LIST', _, '-ERR not implemented\r~n').

client('PAUSE', _, '-ERR not implemented\r~n').

client('SETNAME', _, '-ERR not implemented\r~n').


command('COUNT', _, '-ERR not implemented\r~n').

command('GETKEYS', _, '-ERR not implemented\r~n').

command('INFO', _, '-ERR not implemented\r~n').


config('GET', [Key], R) :-
    atom_string(AKey, Key),
    setting(prodis:AKey, Value),
    term_output([AKey, Value], R).

config('REWRITE', [], '+OK\r~n') :-
    setting(prodis:conffile, Settings),
    load_settings(Settings),
    setting(prodis:logfile, LogFile),
	set_logfile(LogFile).

config('SET', [Key, Value], R) :-
    atom_string(AKey, Key),
    set_setting(prodis:AKey, Value), !,
    term_output([AKey, Value], R).

config('SET', _, '-ERR Unsupported CONFIG parameter\r~n').

config('RESETSTAT', _, '-ERR not implemented\r~n').


p_append(Value, Current, R) :-
    atom(Current), !,
    atom_codes(Current, CurrentCode),
    string_codes(Value, Value1),
    concat(CurrentCode, Value1, Response),
    string_codes(R, Response).

p_append(Value, Current, R) :-
    number(Current), !,
    format(atom(Aux), '~w', [Current]),
    p_append(Value, Aux, R).

p_append(Value, Current, R) :-
    string(Current), !,
    string_codes(Value, Value1),
    string_codes(Current, Current1),
    append(Current1, Value1, Response),
    string_codes(R, Response).

p_append(Value, Current, R) :-
    is_list(Current), !,
    string_codes(Value, Value1),
    append(Current, Value1, Response),
    string_codes(R, Response).

% vim:set et:syntax=prolog
