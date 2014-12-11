%% -*- erlang -*-
Definitions.

D = [0-9]

Rules.

{D}+\.{D}+\.{D}+\.{D}+  : {token, {'IP', TokenLine, ?debug(TokenChars)}}.
{D}+                    : {token, {'INT', TokenLine, list_to_integer(?debug(TokenChars))}}.
[\(\)\[\]\.\,\"\'\=]    : {token, {list_to_atom(?debug(TokenChars)), TokenLine}}.
(\#.*\\n)               : {token, {'COMMENT', TokenLine, ?debug(TokenChars)}}.
([\s\t\r\n]+)           : skip_token. % white space

(\"((\$|[^\"]*)*(\"\")*)*\") : {token, {'DQSTRING', TokenLine, TokenChars}}.

[A-Za-z0-9_\-]* : match_any(TokenChars, TokenLen, TokenLine, ?TokenPatters).

Erlang code.

-define(TokenPatters, [
 {"^(?i)(A)$",                           'A'},
 {"^(?i)(HS)$",                          'HS'},
 {"^(?i)(UR)$",                          'UR'},
 {"^(?i)(NO)$",                          'NO'},
 {"^(?i)(ON)$",                          'ON'},
 {"^(?i)(OK)$",                          'OK'},
 {"^(?i)(SID)$",                         'SID'},
 {"^(?i)(SDU)$",                         'SDU'},
 {"^(?i)(KEY)$",                         'KEY'},
 {"^(?i)(IPC)$",                         'IPC'},
 {"^(?i)(SPX)$",                         'SPX'},
 {"^(?i)(NMP)$",                         'NMP'},
 {"^(?i)(BEQ)$",                         'BEQ'},
 {"^(?i)(YES)$",                         'YES'},
 {"^(?i)(OFF)$",                         'OFF'},
 {"^(?i)(TCP)$",                         'TCP'},
 {"^(?i)(TYPE)$",                        'TYPE'},
 {"^(?i)(NONE)$",                        'NONE'},
 {"^(?i)(HOST)$",                        'HOST'},
 {"^(?i)(PORT)$",                        'PORT'},
 {"^(?i)(TRUE)$",                        'TRUE'},
 {"^(?i)(PIPE)$",                        'PIPE'},
 {"^(?i)(ARGS)$",                        'ARGS'},
 {"^(?i)(BASIC)$",                       'BASIC'},
 {"^(?i)(DELAY)$",                       'DELAY'},
 {"^(?i)(LOCAL)$",                       'LOCAL'},
 {"^(?i)(FALSE)$",                       'FALSE'},
 {"^(?i)(ARGV0)$",                       'ARGV0'},
 {"^(?i)(IFILE)$",                       'IFILE'},
 {"^(?i)(METHOD)$",                      'METHOD'},
 {"^(?i)(SELECT)$",                      'SELECT'},
 {"^(?i)(SERVER)$",                      'SERVER'},
 {"^(?i)(BACKUP)$",                      'BACKUP'},
 {"^(?i)(SHARED)$",                      'SHARED'},
 {"^(?i)(POOLED)$",                      'POOLED'},
 {"^(?i)(ENABLE)$",                      'ENABLE'},
 {"^(?i)(BROKEN)$",                      'BROKEN'},
 {"^(?i)(SESSION)$",                     'SESSION'},
 {"^(?i)(RETRIES)$",                     'RETRIES'},
 {"^(?i)(PROGRAM)$",                     'PROGRAM'},
 {"^(?i)(ADDRESS)$",                     'ADDRESS'},
 {"^(?i)(SERVICE)$",                     'SERVICE'},
 {"^(?i)(FAILOVER)$",                    'FAILOVER'},
 {"^(?i)(PROTOCOL)$",                    'PROTOCOL'},
 {"^(?i)(SECURITY)$",                    'SECURITY'},
 {"^(?i)(DEDICATED)$",                   'DEDICATED'},
 {"^(?i)(PRECONNECT)$",                  'PRECONNECT'},
 {"^(?i)(RETRY_COUNT)$",                 'RETRY_COUNT'},
 {"^(?i)(GLOBAL_NAME)$",                 'GLOBAL_NAME'},
 {"^(?i)(DESCRIPTION)$",                 'DESCRIPTION'},
 {"^(?i)(LOAD_BALANCE)$",                'LOAD_BALANCE'},
 {"^(?i)(RDB_DATABASE)$",                'RDB_DATABASE'},
 {"^(?i)(CONNECT_DATA)$",                'CONNECT_DATA'},
 {"^(?i)(ADDRESS_LIST)$",                'ADDRESS_LIST'},
 {"^(?i)(SOURCE_ROUTE)$",                'SOURCE_ROUTE'},
 {"^(?i)(SERVICE_NAME)$",                'SERVICE_NAME'},
 {"^(?i)(INSTANCE_NAME)$",               'INSTANCE_NAME'},
 {"^(?i)(FAILOVER_MODE)$",               'FAILOVER_MODE'},
 {"^(?i)(RECV_BUF_SIZE)$",               'RECV_BUF_SIZE'},
 {"^(?i)(SEND_BUF_SIZE)$",               'SEND_BUF_SIZE'},
 {"^(?i)(CONNECT_TIMEOUT)$",             'CONNECT_TIMEOUT'},
 {"^(?i)(TYPE_OF_SERVICE)$",             'TYPE_OF_SERVICE'},
 {"^(?i)(DESCRIPTION_LIST)$",            'DESCRIPTION_LIST'},
 {"^(?i)(SSL_SERVER_CERT_DN)$",          'SSL_SERVER_CERT_DN'},
 {"^(?i)(TRANSPORT_CONNECT_TIMEOUT)$",   'TRANSPORT_CONNECT_TIMEOUT'}
]).

match_any(TokenChars, TokenLen, _TokenLine, []) ->
    {token, {'ID', TokenLen, TokenChars}};
match_any(TokenChars, TokenLen, TokenLine, [{P,T}|TPs]) ->
    case re:run(TokenChars, P, [{capture, first, list}]) of
        {match,[_]} -> {token, {T, TokenLine}};
        nomatch -> match_any(TokenChars, TokenLen, TokenLine, TPs)
    end.

-define(debug(T), T).
%-define(debug(T), begin io:format(user, "Token ~p~n", [T]), T end).
