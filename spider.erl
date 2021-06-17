-module(spider).
-export([start_link/2, stop/1, enable/1, disable/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-behaviour(gen_server).

-record(spider_state, {working_status = disabled, 
                        action = idle, 
                        url = url,
                        cooldown = 10000}).

%% behavior
start_link(Url, Cooldown) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Url, Cooldown}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init({Url, Cooldown}) ->
    self() ! perform,
    {ok, #spider_state{working_status = disabled, url=Url, action=idle, cooldown = Cooldown}}.

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
handle_info(perform, SpiderState = #spider_state{working_status=disabled}) ->
    % self() ! perform,
    {noreply, SpiderState};

handle_info(perform, #spider_state{working_status=enabled, cooldown=Ms, url=Url, action=idle}) ->
    
    io:format("woke up, went to is ~p~n", [Url]),
    self() ! perform,
    {noreply, #spider_state{working_status=enabled, cooldown = Ms, url = Url, action = traverse}};


handle_info(perform, #spider_state{working_status=enabled, url=Url, cooldown=Ms, action=traverse}) ->

    io:format("traversing~n"),
    % catch errors - send to action failure - coming soon, it's almost 1 am, for christ sake, i wanna play wolfenstein
    io:format("~p~n", [Url]),
    {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(get, Url, [], <<>>, []),
    io:format("Status: ~p~n", [StatusCode]),
    self() ! perform,
    {noreply, #spider_state{working_status=enabled, cooldown = Ms, url=Url, action = success}};


handle_info(perform, #spider_state{working_status=enabled, cooldown=Cooldown, url=Url, action=success}) ->

    io:format("done traversing~n"),
    io:format("next traverse in ~p~n", [Cooldown]),
    {ok, _Timer} = timer:send_after(Cooldown, self(), perform),
    {noreply, #spider_state{working_status=enabled, cooldown = Cooldown, url=Url, action = idle}}.


%% calls
handle_call(enable, _From, SpiderState = #spider_state{working_status = enabled}) ->
    {reply, {ok, already_enabled}, SpiderState};


handle_call(enable, _From, #spider_state{working_status = disabled, url=Url, cooldown=Cooldown}) ->
    self() ! perform,
    {reply, {ok, enabled}, #spider_state{working_status=enabled, url=Url, cooldown = Cooldown}};


handle_call(disable, _From, #spider_state{working_status = enabled, action = idle, url=Url, cooldown=Cooldown}) ->
    {reply, {ok, disabled}, #spider_state{working_status=disabled, url=Url, cooldown=Cooldown}};


handle_call(disable, _From, SpiderState = #spider_state{working_status = disabled}) ->
    {reply, {ok, already_disabled}, SpiderState};


handle_call(stop, _From, SpiderState = #spider_state{working_status = enabled}) ->
    {reply, {ok, cant_stop_enabled}, SpiderState};


handle_call(stop, _From, SpiderState = #spider_state{working_status = disabled}) ->
    {stop, normal, ok, SpiderState}.