%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree_update_req).

-export([handle_req/2]).

-include("braintree.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_bookkeepers:update_req_v(JObj),
    %% io:format("update: ~p~n", [JObj]),
    Response = [{<<"test">>, <<"yup">>}
                | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
               ],
    RespQ = kz_json:get_value(<<"Server-ID">>, JObj),
    kz_amqp_worker:cast(Response, fun(P) -> kapi_bookkeepers:publish_update_resp(RespQ, P) end),
    'ok'.
