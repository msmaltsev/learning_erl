-module(spider).
-export([start_link/0, stop/1, enable/1, disable/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-behaviour(gen_server).

-record(spider_state, {working_status = disabled, 
                        action = idle, 
                        cooldown = 10000}).

% parameters
url() -> <<"https://lenta.ru/rss/">>.
cooldown() -> 10 * 1000.

%% behavior

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, params, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init(params) ->
    {ok, _Timer} = timer:send_after(1000, self(), perform),
    {ok, #spider_state{working_status = disabled, cooldown = 10000}}.

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
    self() ! perform,
    {noreply, SpiderState};


handle_info(perform, #spider_state{working_status=enabled, cooldown=Ms, action = Action}) ->

    case Action of 

        idle ->
            io:format("woke up, went to is ~p~n", [url()]),
            self() ! perform,
            {noreply, #spider_state{working_status=enabled, cooldown = Ms, action = traverse}};

        traverse ->
            io:format("traversing~n"),
            % catch errors - send to failure


            {ok, StatusCode, RespHeaders, ClientRef} = hackney:get(url(), [], <<>>, []),
            io:format("Status: ~p~n", [StatusCode]),
            {ok, Body} = hackney:body(ClientRef),
            io:format(Body),

            self() ! perform,
            {noreply, #spider_state{working_status=enabled, cooldown = Ms, action = success}};
        success ->
            io:format("done traversing~n"),
            io:format("next traverse in ~p~n", [cooldown()]),
            {ok, _Timer} = timer:send_after(cooldown(), self(), perform),
            {noreply, #spider_state{working_status=enabled, cooldown = cooldown(), action = idle}}
    end.


%% calls

handle_call(enable, _From, SpiderState = #spider_state{working_status = WorkingStatus}) ->
    case WorkingStatus of
        enabled -> 
            {reply, {ok, already_enabled}, SpiderState};
        disabled -> 
            {reply, {ok, enabled}, #spider_state{working_status=enabled}}
    end;


handle_call(disable, _From, SpiderState = #spider_state{working_status = WorkingStatus, action = Action}) ->

    case Action of 
        idle ->
            case WorkingStatus of
                enabled -> 
                    {reply, {ok, disabled}, #spider_state{working_status=disabled}};
                disabled ->
                    {reply, {ok, already_disabled}, SpiderState}
            end;
        _ -> 
            {reply, {ok, busy}, SpiderState}
    end;


handle_call(stop, _From, SpiderState = #spider_state{working_status = WorkingStatus, action = Action}) ->

    case Action of 

        idle ->

            case WorkingStatus of
                enabled ->
                    {reply, {ok, cant_stop_enabled}, SpiderState};
                disabled -> 
                    {stop, normal, ok, SpiderState}
            end;

        _ ->
            {reply, {ok, busy}, SpiderState}

    end.
