-module(spider).
-export([start_link/3, stop/1, enable/1, disable/1, start_link/0, month_name_to_integer/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-behaviour(gen_server).

-record(spider_state, {working_status = disabled, 
                        action = idle, 
                        url = url,
                        cooldown = 10000, 
                        most_recent = {{0,0,0},{0,0,0}}
                    }).

-include_app("hackney/include/hackney_lib.hrl").
-include_lib("xmerl/include/xmerl.hrl").


%% behavior
start_link(Url, Cooldown, MostRecent) ->
    gen_server:start_link(?MODULE, {Url, Cooldown, MostRecent}, []).

start_link() ->
    Url = "https://meduza.io/rss2/all",
    Cooldown = 5000,
    MostRecent = {{2021,6,25}, {0,0,0}},
    io:format("launching with parameters: ~p, ~p, ~p~n", [Url, Cooldown, MostRecent]),
    gen_server:start_link(?MODULE, {Url, Cooldown, MostRecent}, []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init({Url, Cooldown, MostRecent}) ->
    self() ! perform,
    {ok, #spider_state{working_status = enabled, url=Url, action=idle, most_recent = MostRecent, cooldown = Cooldown}}.

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

month_name_to_integer(<<"Jan">>) -> 1;
month_name_to_integer(<<"Feb">>) -> 2;
month_name_to_integer(<<"Mar">>) -> 3;
month_name_to_integer(<<"Apr">>) -> 4;
month_name_to_integer(<<"May">>) -> 5;
month_name_to_integer(<<"Jun">>) -> 6;
month_name_to_integer(<<"Jul">>) -> 7;
month_name_to_integer(<<"Aug">>) -> 8;
month_name_to_integer(<<"Sep">>) -> 9;
month_name_to_integer(<<"Oct">>) -> 10;
month_name_to_integer(<<"Nov">>) -> 11;
month_name_to_integer(<<"Dec">>) -> 12.

parse_rfc_time(Dt_str) ->
    <<_:5/binary, D:2/binary, _:1/binary, M:3/binary, _:1/binary, Y:4/binary, _:1/binary, H:2/binary, _:1/binary, Mi:2/binary, _:1/binary, S:2/binary, _/binary>> = list_to_binary(Dt_str),
    {{
        list_to_integer(binary_to_list(Y)), 
        month_name_to_integer(M), 
        list_to_integer(binary_to_list(D))
    }, {
        list_to_integer(binary_to_list(H)), 
        list_to_integer(binary_to_list(Mi)), 
        list_to_integer(binary_to_list(S))
    }}.

%% info
handle_info(perform, SpiderState = #spider_state{working_status=disabled}) ->
    % self() ! perform,
    {noreply, SpiderState};

handle_info(perform, #spider_state{working_status=enabled, cooldown=Ms, url=Url, most_recent = MostRecent, action=idle}) ->
    
    io:format("woke up, target url is ~p~n", [Url]),
    self() ! perform,
    {noreply, #spider_state{working_status=enabled, cooldown = Ms, url = Url, most_recent = MostRecent, action = traverse}};


handle_info(perform, #spider_state{working_status=enabled, url=Url, cooldown=Ms, most_recent = MostRecent, action=traverse}) ->

    io:format("traversing to ~p. most_recent is ~p~n", [Url, MostRecent]),
    {ok, Platform} = maps:find(host, uri_string:parse(Url)),
    {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(get, Url, [], <<>>, []),
    io:format("Status: ~p~n", [StatusCode]),
    {ok, Body} = hackney:body(ClientRef),

    % Filename = Platform ++ integer_to_list(erlang:system_time()) ++ ".txt",
    % file:write_file(Filename, [Body]),

    {Xml, _} = xmerl_scan:string(binary_to_list(Body)),
    % FeedTitle = xmerl_xpath:string("/rss/channel/title/text()", Xml),
    % io:format("~p~n", [FeedTitle]),

    Items = xmerl_xpath:string("/rss/channel//item", Xml),
    % io:format("~p~n", [Items]),

    Docs = lists:flatmap(fun(Item) -> % generating doc from each item

        [{xmlText,[_|_],1,[],DocUrl,_}] = xmerl_xpath:string("//link/text()", Item),
        % io:format("~p~n", [DocUrl]),
        [{xmlText,[_|_],1,[],DocTime,_}] = xmerl_xpath:string("//pubDate/text()", Item),
        % io:format("~p~n", [DocDate]),
        [{xmlText,[_|_],1,[],DocBody,_}] = xmerl_xpath:string("//description/text()", Item),
        % io:format("~p~n", [DocBody]),
        [{xmlText,[_|_],1,[],DocTitle,_}] = xmerl_xpath:string("//title/text()", Item),
        % io:format("~p~n", [DocTitle]),
        [{xmlAttribute,url,[],[],[],[_|_],1,[],EnclosureUrl,_}] = xmerl_xpath:string("//enclosure/@url", Item),
        % io:format("~p~n", [EnclosureUrl]),

        case xmerl_xpath:string("//author/text()", Item) of
            [] -> 
                % io:format("AUTHOR: ~p~n", [Platform]);
                DocAuthor = Platform;
            _ ->
                [{xmlText,[_|_],1,[],DocAuthor,_}] = xmerl_xpath:string("//author/text()", Item)
        end,

        NiceTime = parse_rfc_time(DocTime),
        % io:format("NiceTime ~p~n", [NiceTime]),
        % io:format("MostRecent ~p~n", [MostRecent]),

        % io:format("~p~n", [is_list(DocBody)]),
        % NiceBody = list_to_binary(DocBody),
        % io:format("~p~n", [NiceBody]),
        % NiceTitle = list_to_binary(DocTitle),

        case NiceTime of 
            N when N > MostRecent ->
                [#{
                    title => DocTitle,
                    body => DocBody,
                    url => DocUrl, 
                    time => NiceTime,
                    author_name => DocAuthor,
                    author_extid => DocAuthor,
                    images => [EnclosureUrl],
                    type => "p"
                }];
            N when N =< MostRecent -> []
        end

        end, Items),

    % io:format("~p~n", [AllDocuments]),

    Times = lists:map(fun(Doc) ->
        maps:get(time, Doc)
        end, Docs),

    case Times of
        [] -> 
            NewMostRecent = MostRecent;
        _ ->
            NewMostRecent = lists:max(Times)
        end,

    % io:format("~p~n", [Times]),
    % NewDocs = lists:filter(fun(Doc) ->
    %     maps:get(time, Doc) > MostRecent
    %     end, Docs),

    % io:format("~p~n", [Docs]),
    io:format("~p docs downloaded~n", [lists:flatlength(Docs)]),
    % io:format("~p~n", [Docs]),

    case Docs of 
        [] -> 
            io:format("no docs to write~n");
        _ ->
            Filename = Platform ++ "." ++ integer_to_list(erlang:system_time()) ++ ".txt",

            {ok, F} = file:open(Filename, [write]),
            io:format(F, "~p~n", [Docs]),
            io:format("wrote docs to file ~p~n", [Filename])
        end,


    self() ! perform,
    {noreply, #spider_state{working_status=enabled, cooldown = Ms, url=Url, most_recent = NewMostRecent, action = success}};


handle_info(perform, #spider_state{working_status=enabled, cooldown=Cooldown, url=Url, most_recent = MostRecent, action=success}) ->

    io:format("done traversing ~p, next traverse in ~p. newest doc from ~p~n~n~n", [Url, Cooldown, MostRecent]),
    {ok, _Timer} = timer:send_after(Cooldown, self(), perform),
    {noreply, #spider_state{working_status=enabled, cooldown = Cooldown, url=Url, most_recent = MostRecent, action = idle}}.


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


% handle_call(stop, _From, SpiderState = #spider_state{working_status = enabled}) ->
%     {reply, {ok, cant_stop_enabled}, SpiderState};


handle_call(stop, _From, SpiderState) ->
    {stop, normal, ok, SpiderState}.