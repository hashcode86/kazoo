%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(braintree_quick_sale).

-export([handle_req/2]).

-include("braintree.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_bookkeepers:quick_sale_req_v(JObj),
    'ok'.
