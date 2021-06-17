-module(spider_sup).

-export([start_link/0, init/1]).
-behavior(supervisor).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).


init({}) ->
    {ok, { 
        #{
            strategy => one_for_one, 
            intensity => 400,
            period => 10
        },
    
        [
            #{ id => lenta,
                start => { spider, start_link, ["https://lenta.ru/", 8000]}
            },

            #{ id => vk,
                start => { spider, start_link, ["https://vk.com/", 10000]}
            }
        ]
    }}.
%% 400 crasher per 10 sec
