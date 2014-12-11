%% -*- erlang -*-
Header "%% Copyright (C) "
"%% @private"
"%% @Author Bikram Chatterjee"
"%% @Email razorpeak@gmail.com".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals
 tns tnsname tns_entry ifile lsnr_entry alias_list description_list description
 alias lsnr_rval lsnr_description address_list addresses address
 addrs_or_addr_list opt_al_params al_param al_load_balance al_failover
 al_source_route boolean a_params protocol_info tcp_protocol tcp_params
 tcp_parameter ssn basic_preconn fo_parameter fo_params id rdb_db dsp
 cd_params cd_parameter bad_parameter bad_params beq_protocol beq_params
 beq_proto connect_data beq_parameter a_parameter ipc_protocol ipc_params
 ipc_parameter d_send_buf d_recv_buf spx_parameter nmp_protocol host ids
 spx_protocol spx_params nmp_params nmp_parameter d_params d_param
 ds_parameter dl_parameter opt_dl_params dl_params al_params opt_a_params
 opt_d_params.

Terminals
 ID DQSTRING DESCRIPTION HS UR NO ON OK SID SDU KEY IPC SPX NMP BEQ YES OFF TCP
 TYPE NONE HOST PORT TRUE PIPE ARGS BASIC DELAY LOCAL FALSE ARGV0 IFILE METHOD
 SELECT SERVER BACKUP SHARED POOLED ENABLE BROKEN SESSION RETRIES PROGRAM
 ADDRESS SERVICE FAILOVER PROTOCOL SECURITY DEDICATED PRECONNECT RETRY_COUNT
 GLOBAL_NAME LOAD_BALANCE RDB_DATABASE CONNECT_DATA ADDRESS_LIST SOURCE_ROUTE
 SERVICE_NAME INSTANCE_NAME FAILOVER_MODE RECV_BUF_SIZE SEND_BUF_SIZE
 CONNECT_TIMEOUT TYPE_OF_SERVICE DESCRIPTION_LIST SSL_SERVER_CERT_DN INT UR_A
 TRANSPORT_CONNECT_TIMEOUT IP
 '(' ')' '[' ']' '=' '.' ',' '\''.

Rootsymbol tns.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tns -> tnsname      : ['$1'].
tns -> tnsname tns  : ['$1'|'$2'].

tnsname -> tns_entry   : {tns, '$1'}.
tnsname -> ifile       : {ifile, '$1'}.
tnsname -> lsnr_entry  : {listener, '$1'}.

tns_entry -> alias_list '=' description_list : {'$1', '$3'}.

alias_list -> alias                 : ['$1']. 
alias_list -> alias ',' alias_list  : ['$1'|'$3'].

alias -> ID             : [unwrap('$1')].
alias -> ID '.' alias   : [unwrap('$1')|'$3'].

ifile -> IFILE '=' DQSTRING   : {ifile, unwrap('$3')}.

lsnr_entry -> alias '=' lsnr_rval   : {lsnr, '$1', '$3'}.

lsnr_rval -> lsnr_description    : '$1'.
lsnr_rval -> addrs_or_addr_list  : '$1'.

lsnr_description -> '(' DESCRIPTION '=' addrs_or_addr_list ')'
                    : {lsnr_desc, '$4'}.

description_list -> '(' DESCRIPTION_LIST '=' opt_dl_params description_list
                    opt_dl_params ')'
                    : {desc_list, '$4', '$5', '$6'}.

opt_dl_params -> '$empty'           : undefined.
opt_dl_params -> dl_params          : '$1'.
dl_params -> dl_parameter           : ['$1'].
dl_params -> dl_parameter dl_params : ['$1'|'$2'].

dl_parameter -> al_failover     : '$1'.
dl_parameter -> al_load_balance : '$1'.
dl_parameter -> al_source_route : '$1'.

description_list -> description                     : ['$1'].
description_list -> description description_list    : ['$1'|'$2'].

description -> '(' DESCRIPTION '=' opt_d_params addrs_or_addr_list opt_d_params
               connect_data opt_d_params ')'
               : {description, '$4', '$5', '$6', '$7', '$8'}.

opt_d_params -> '$empty'            : undefined.
opt_d_params -> d_params            : '$1'.
d_params -> d_param                 : ['$1'].
d_params -> d_param d_params        : ['$1'|'$2'].

d_param -> '(' ENABLE '=' BROKEN ')'                    : {enable, broken}.
d_param -> al_failover                                  : '$1'.
d_param -> al_load_balance                              : '$1'.
d_param -> '(' SDU '=' INT ')'                          : {sdu, unwrap('$4')}.
d_param -> d_recv_buf                                   : '$1'.
d_param -> d_send_buf                                   : '$1'.
d_param -> al_source_route                              : '$1'.
d_param -> '(' SECURITY '=' ds_parameter ')'            : {security, '$4'}.
d_param -> '(' RETRY_COUNT '=' INT ')'                  : {retry, unwrap('$4')}.
d_param -> '(' TYPE_OF_SERVICE '=' ID ')'
           : {service_type, unwrap('$4')}.
d_param -> '(' CONNECT_TIMEOUT '=' INT ')'
           : {conn_to, unwrap('$4')}.
d_param -> '(' TRANSPORT_CONNECT_TIMEOUT '=' INT ')'
           : {trans_conn_to, unwrap('$4')}.

d_recv_buf -> '(' RECV_BUF_SIZE '=' INT ')' : {rcv_buf, unwrap('$4')}.
d_send_buf -> '(' SEND_BUF_SIZE '=' INT ')' : {snd_buf, unwrap('$4')}.

ds_parameter -> '(' SSL_SERVER_CERT_DN '=' DQSTRING ')'
                : {ssl, unwrap('$5')}.

addrs_or_addr_list -> address_list  : '$1'.
addrs_or_addr_list -> addresses     : '$1'.
                          
addresses -> address            : ['$1'].
addresses -> address addresses  : ['$1'|'$2'].

address_list -> '(' ADDRESS_LIST '=' opt_al_params addresses opt_al_params ')'
                : {addr_list, '$4', '$5', '$6'}.

opt_al_params -> '$empty'               : undefined.
opt_al_params -> al_params              : '$1'.
al_params -> al_param                   : ['$1'].
al_params -> al_param al_params         : ['$1'|'$2'].

al_param -> al_failover     : '$1'.
al_param -> al_load_balance : '$1'.
al_param -> al_source_route : '$1'.

al_failover -> '(' FAILOVER '=' boolean ')' : {failover, '$4'}.
al_load_balance -> '(' LOAD_BALANCE '=' boolean ')' : {load_balance, '$4'}.
al_source_route -> '(' SOURCE_ROUTE '=' boolean ')' : {source_route, '$4'}.

boolean -> YES      : unwrap('$1').
boolean -> NO       : unwrap('$1').
boolean -> ON       : unwrap('$1').
boolean -> OFF      : unwrap('$1').
boolean -> TRUE     : unwrap('$1').
boolean -> FALSE    : unwrap('$1').

address -> '(' ADDRESS '='  protocol_info opt_a_params ')'
           : {address, '$4', '$5'}.

opt_a_params -> '$empty'            : undefined.
opt_a_params -> a_params            : '$1'.
a_params -> a_parameter             : ['$1'].
a_params -> a_parameter a_params    : ['$1'|'$2'].

a_parameter -> d_send_buf   : '$1'.
a_parameter -> d_recv_buf   : '$1'.

% See (for examples etc):
% http://www.toadworld.com/platforms/oracle/w/wiki/5484.defining-tnsname-addresses.aspx
protocol_info -> tcp_protocol   : '$1'.
protocol_info -> ipc_protocol   : '$1'.
protocol_info -> spx_protocol   : '$1'.
protocol_info -> nmp_protocol   : '$1'.
protocol_info -> beq_protocol   : '$1'.

tcp_protocol -> tcp_params                  : {tcp, '$1'}.
tcp_params -> tcp_parameter                 : ['$1'].
tcp_params -> tcp_parameter tcp_params      : ['$1'|'$2'].
tcp_parameter -> '(' HOST '=' host ')'      : {host, '$4'}.
tcp_parameter -> '(' PORT '=' INT ')'       : {port, unwrap('$4')}.
tcp_parameter -> '(' PROTOCOL '=' TCP ')'   : {protocol, tcp}.
host -> ids                                 : '$1'.
host -> IP                                  : {ip, unwrap('$1')}.
ids -> ID                                   : [unwrap('$1')].
ids -> ID '.' ids                           : [unwrap('$1')|'$3'].

ipc_protocol -> ipc_params                  : {ipc, '$1'}.
ipc_params -> ipc_parameter                 : ['$1'].
ipc_params -> ipc_parameter ipc_params      : ['$1'|'$2'].
ipc_parameter -> '(' PROTOCOL '=' IPC ')'   : {protocol, ipc}.
ipc_parameter -> '(' KEY '=' ID ')'         : {key, unwrap('$4')}.

spx_protocol -> spx_params                  : {spx, '$1'}.
spx_params -> spx_parameter                 : ['$1'].
spx_params -> spx_parameter spx_params      : ['$1'|'$2'].
spx_parameter -> '(' PROTOCOL '=' SPX ')'   : {protocol, spx}.
spx_parameter -> '(' SERVICE '=' ID ')'     : {service, unwrap('$4')}.

nmp_protocol -> nmp_params                  : {nmp, '$1'}.
nmp_params -> nmp_parameter                 : ['$1'].
nmp_params -> nmp_parameter nmp_params      : ['$1'|'$2'].

nmp_parameter -> '(' PROTOCOL '=' NMP ')'   : {protocol, nmp}.
nmp_parameter -> '(' SERVER '=' ID ')'      : {server, unwrap('$4')}.
nmp_parameter -> '(' PIPE '=' ID ')'        : {pipe, unwrap('$4')}.
                 

beq_protocol -> beq_params                      : {beq, '$1'}.
beq_proto -> '(' PROTOCOL '=' BEQ ')'           : {protocol, beq}.
beq_params -> beq_parameter                     : ['$1'].
beq_params -> beq_parameter beq_params          : ['$1'|'$2'].
beq_parameter -> beq_proto                      : '$1'.
beq_parameter -> '(' PROGRAM '=' ID ')'         : {program, unwrap('$4')}.
beq_parameter -> '(' ARGV0 '=' ID ')'           : {argv0, unwrap('$4')}.
beq_parameter -> '(' ARGS '=' '\'' '(' DESCRIPTION '=' bad_params ')' '\'' ')'
                 : {args, '$4'}.
bad_params -> bad_parameter                     : ['$1'].
bad_params -> bad_parameter bad_params          : ['$1'|'$2'].
bad_parameter -> '(' LOCAL '=' boolean ')'      : {local, '$4'}.
bad_parameter -> '(' ADDRESS '=' beq_proto ')'  : {address, '$4'}.


connect_data -> '(' CONNECT_DATA '=' cd_params ')' : {connect_data, '$4'}.

cd_params -> cd_parameter           : ['$1'].
cd_params -> cd_parameter cd_params : ['$1'|'$2'].
                 
cd_parameter -> '(' SERVICE_NAME '=' id ')'         : {service, '$4'}.
cd_parameter -> '(' SID '=' ID ')'                  : {sid, unwrap('$4')}.
cd_parameter -> '(' INSTANCE_NAME '=' id ')'        : {instance, '$4'}.
cd_parameter -> '(' FAILOVER_MODE '=' fo_params ')' : {fo_mode, '$4'}.
cd_parameter -> '(' GLOBAL_NAME '=' id ')'          : {global, '$4'}.
cd_parameter -> '(' HS '=' OK ')'                   : {hs, ok}.
cd_parameter -> '(' RDB_DATABASE '=' rdb_db ')'     : {rdb_db, '$4'}.
cd_parameter -> '(' SERVER '=' dsp ')'              : {server, '$4'}.
cd_parameter -> '(' UR '=' UR_A ')'                 : {ur, a}.

dsp -> DEDICATED    : dedicated.
dsp -> SHARED       : shared.
dsp -> POOLED       : pooled.

rdb_db -> id                : {id, '$1'}.
rdb_db -> '[' '.' ID ']' id : {id, [{pre,unwrap('$3')}|'$5']}.

id -> ID        : [unwrap('$1')].
id -> ID '.' id : ['$1'|'$3'].

fo_params -> fo_parameter           : ['$1'].
fo_params -> fo_parameter fo_params : ['$1'|'$2'].

fo_parameter -> '(' TYPE '=' ssn ')'                : {type, '$4'}.
fo_parameter -> '(' BACKUP '=' id ')'               : {backup, '$4'}.
fo_parameter -> '(' METHOD '=' basic_preconn ')'    : {method, '$4'}.
fo_parameter -> '(' RETRIES '=' INT ')'             : {retires, unwrap('$4')}.
fo_parameter -> '(' DELAY '=' INT ')'               : {delay, unwrap('$4')}.

ssn -> SESSION  : session.
ssn -> SELECT   : select.
ssn -> NONE     : none.

basic_preconn -> BASIC      : basic.
basic_preconn -> PRECONNECT : preconnect.
                 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Erlang code.

-behaviour(application).
-behaviour(supervisor).

% application callbacks
-export([start/0, start/2, stop/1, stop/0]).

% Supervisor callbacks
-export([init/1]).

% parser and compiler interface
-export([parsetree/1, parsetree_with_tokens/1
         %, foldtd/3, foldbu/3
         %, string/1, roots/1
        ]).

%%-----------------------------------------------------------------------------
%%                          dummy application interface
%%-----------------------------------------------------------------------------

start()             -> application:start(?MODULE).
stop()              -> application:stop(?MODULE).

start(_Type, _Args) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_State)        -> ok.
init([])            -> {ok, { {one_for_one, 5, 10}, []} }.

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------

unwrap({_,_,X}) -> X.

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  PARSER
%%-----------------------------------------------------------------------------
-spec parsetree(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, tuple()}.
parsetree(Tns) ->
   case parsetree_with_tokens(Tns) of
       {ok, {ParseTree, _Tokens}} -> {ok, ParseTree};
       Error -> Error
   end.

-spec parsetree_with_tokens(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, {tuple(), list()}}.
parsetree_with_tokens(Tns) when is_binary(Tns) ->
    parsetree_with_tokens(binary_to_list(Tns));
parsetree_with_tokens([]) -> {parse_error, {not_a_valid_json_path, []}};
parsetree_with_tokens(Tns) when is_list(Tns) ->
    catch application:ensure_started(?MODULE),
    case tnsnames_tok:string(Tns) of
        {ok, Toks, _} ->
            case ?MODULE:parse(Toks) of
                {ok, PTree} -> {ok, {PTree, Toks}};
                {error, {Line, ?MODULE, Message}} ->
                    {parse_error,
                     {Line, lists:flatten(?MODULE:format_error(Message)), Toks}}
            end;
        LexErrorInfo -> {lex_error, tnsnames_tok:format_error(LexErrorInfo)}
    end;
parsetree_with_tokens(SomethingElse) ->
    {parse_error, {not_a_valid_tns, SomethingElse}}.

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  COMPILER
%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%-----------------------------------------------------------------------------
%%                               EUnit test
%%-----------------------------------------------------------------------------

parse_test() ->
    ?debugMsg("==========================================="),
    ?debugMsg("|    T N S   N A M E S   P A R S I N G    |"),
    ?debugMsg("==========================================="),
    catch application:start(?MODULE),
    Cwd = filename:absname(""),
    {ShowParseTree, Tests} =
        case file:consult(filename:join([Cwd, "..", "test", "test.txt"])) of
            {ok, [show_parse_tree, T]}  -> {true, T};
            {ok, [_, T]}                -> {false, T};
            {ok, [T]}                   -> {false, T};
            {error, Error}              -> ?assertEqual(ok, Error)
        end,
    ?debugFmt("Test result ~p parse tree"
              , [if ShowParseTree -> with; true -> without end]),
    test_parse(1, ShowParseTree, Tests).

test_parse(_, _, []) -> ok;
test_parse(N, ShowParseTree, [{Test,Target}|Tests]) when is_binary(Test) ->
    test_parse(N, ShowParseTree, [{binary_to_list(Test),Target}|Tests]);
test_parse(N, ShowParseTree, [{Test,Target}|Tests]) ->
    ?debugFmt("[~p]----------------------------------------",[N]),
    ?debugFmt("~ts", [Test]),
    {Tokens,EndLine} = case t_tokenize(Test) of
        {ok,T,E} -> {T,E};
        {error, Error} ->
            ?debugFmt("Tokenize Error ~p", [Error]),
            ?assertEqual(ok, tokenize_error)
    end,
    PTree = case t_parse(Tokens) of
        {ok, PT} -> PT;
        {error, {Line, PError}} ->
            ?debugFmt("Parse Error at ~p : ~s", [Line, PError]),
            ?debugFmt("Tokens ~p:~p", [EndLine,Tokens]),
            ?assertEqual(ok, parsing_error)
    end,
    ?assertEqual(Target, PTree),
    if ShowParseTree -> ?debugFmt("~p", [PTree]); true -> ok end,
    FoldTest = case tnsnames:string(PTree) of
        {ok, Ft} -> Ft;
        {error, FError} ->
            ?debugFmt("Folding Error : ~p", [FError]),
            ?debugFmt("ParseTree :~p", [PTree]),
            ?assertEqual(ok, fold_error)
    end,
    ?assertEqual(Test, binary_to_list(FoldTest)),
    test_parse(N+1, ShowParseTree, Tests).

t_tokenize(Test) ->
    case tnsnames_tok:string(Test) of
        {ok,Tokens,EndLine} -> {ok,Tokens,EndLine};
        ErrorInfo -> {error, tnsnames_tok:format_error(ErrorInfo)}
    end.

t_parse(Tokens) ->
    case tnsnames:parse(Tokens) of
        {ok, PTree} -> {ok, PTree};
        {error, {Line, ?MODULE, Message}} ->
            {error, {Line, ?MODULE:flatten(?MODULE:format_error(Message))}}
    end.

%%-----------------------------------------------------------------------------

-endif.
