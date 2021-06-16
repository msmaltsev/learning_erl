-module(spider).
-export([start_link/0, stop/1, enable/1, disable/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-behaviour(gen_server).

-record(state_idle, {enabled = 0, timeout = 0}).
-record(state_working, {}).
-record(spider_state, {state = #state_idle{enabled = 0, timeout = 0}}).


% parameters
url() -> <<"https://lenta.ru/rss/">>.

%% behavior

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, params, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init(params) ->
    {ok, _Timer} = timer:send_after(1000, self(), perform),
    {ok, #spider_state{state = #state_idle{}}}.

code_change(_OldVsn, SpiderState, _Extra) ->
    {ok, SpiderState}.

terminate(Reason, #spider_state{}) ->
    io:format("terminated because of ~p~n", [Reason]).

handle_cast(_Msg, SpiderState) ->
    {noreply, SpiderState}.


%% actions

enable(Pid) ->
    gen_server:call(Pid, enable).

disable(Pid) ->
    gen_server:call(Pid, disable).
    


%% info

handle_info(perform, #spider_state{state = #state_working{}}) ->
    io:format("traversing~n"),

    {ok, StatusCode, RespHeaders, ClientRef} = hackney:get(url(), [], <<>>, []),
    io:format("Status: ~p~n", [StatusCode]),
    {ok, Body} = hackney:body(ClientRef),

    io:format(Body),

    io:format("done traversing~n"),
    self() ! perform,
    {noreply, #spider_state{state = #state_idle{enabled = 1, timeout = 0}}};


handle_info(perform, SpiderState = #spider_state{state = #state_idle{enabled = Enabled, timeout = Timeout}}) when Timeout < 10000 ->
    io:format("sleeping, timeout ~p~n", [Timeout]),
    case Enabled of 
        0 ->
            {ok, _Timer} = timer:send_after(1000, self(), perform),
            {noreply, SpiderState};
        1 ->
            {ok, _Timer} = timer:send_after(1000, self(), perform),
            {noreply, #spider_state{state = #state_idle{enabled = Enabled, timeout = Timeout + 1000}}}
    end;


handle_info(perform, SpiderState = #spider_state{state = #state_idle{enabled = Enabled, timeout = Timeout}}) when Timeout >= 10000 ->
    % io:format("sleeping, enabled ~p, timeout ~p~n", [Enabled, Timeout]),
    case Enabled of 
        1 ->
            self() ! perform,
            {noreply, #spider_state{state = #state_working{}}};
        0 ->
            self() ! perform,
            {noreply, SpiderState}
    end.
    


%% calls

handle_call(enable, _From, #spider_state{state=#state_idle{enabled = Enabled}}) ->
    case Enabled of
        1 -> 
            {reply, {ok, already_enabled}, #spider_state{state=#state_idle{enabled = 1}}};
        0 -> 
            {reply, {ok, enabled}, #spider_state{state=#state_working{}}}
    end;

handle_call(disable, _From, #spider_state{state=#state_idle{enabled = Enabled}}) ->
    case Enabled of
        1 -> 
            {reply, {ok, disabled}, #spider_state{state=#state_idle{enabled = 0}}};
        0 -> 
            {reply, {ok, already_disabled}, #spider_state{state=#state_idle{enabled = 0, timeout = 0}}}
    end;

handle_call(_, _From, SpiderState = #spider_state{state=#state_working{}}) ->
    {reply, {ok, currently_working}, SpiderState};

handle_call(_, _From, SpiderState = #spider_state{state=#state_idle{}}) ->
    {reply, {ok, dont_understand}, SpiderState}.
