%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_bookkeepers).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([api_definitions/0, api_definition/1]).

-export([quick_sale_req/1, quick_sale_req_v/1
        ,publish_quick_sale_req/1, publish_quick_sale_req/2
        ]).
-export([quick_sale_resp/1, quick_sale_resp_v/1
        ,publish_quick_sale_resp/2, publish_quick_sale_resp/3
        ]).
-export([update_req/1, update_req_v/1
        ,publish_update_req/1, publish_update_req/2
        ]).
-export([update_resp/1, update_resp_v/1
        ,publish_update_resp/2, publish_update_resp/3
        ]).

-include_lib("kz_amqp_util.hrl").

-define(BINDING_STRING(Category, Name), <<"bookkeepers.", (Category)/binary, ".", (Name)/binary>>).

%%%=============================================================================
%%% Internal Bookkeeper Definitions
%%%=============================================================================
-spec quick_sale_req_definition() -> kapi_definition:api().
quick_sale_req_definition() ->
    #kapi_definition{name = <<"quick_sale_req">>
                    ,friendly_name = <<"Quick Sale Request">>
                    ,description = <<"Will trigger the appropriate bookkeeper to withdraw money and add it to the accounts credit">>
                    ,build_fun = fun quick_sale_req/1
                    ,validate_fun = fun quick_sale_req_v/1
                    ,publish_fun = fun publish_quick_sale_req/1
                    ,binding = ?BINDING_STRING(<<"quick_sale">>, <<"request">>)
                    ,restrict_to = 'quick_sale'
                    ,required_headers = [<<"Bookkeeper-Name">>
                                        ,<<"Bookkeeper-ID">>
                                        ,<<"Account-ID">>
                                        ,<<"Vendor-ID">>
                                        ,<<"Amount">>
                                        ]
                    ,optional_headers = []
                    ,values = [{<<"Event-Category">>, <<"bookkeepers">>}
                              ,{<<"Event-Name">>, <<"quick_sale_req">>}
                              ]
                    ,types = []
                    }.

-spec quick_sale_resp_definition() -> kapi_definition:api().
quick_sale_resp_definition() ->
    #kapi_definition{name = <<"quick_sale_resp">>
                    ,friendly_name = <<"Quick Sale Response">>
                    ,description = <<"Result of the attempt to withdraw money via the bookkeeper">>
                    ,build_fun = fun quick_sale_resp/1
                    ,validate_fun = fun quick_sale_resp_v/1
                    ,publish_fun = fun publish_quick_sale_resp/2
                    ,binding = ?BINDING_STRING(<<"quick_sale">>, <<"response">>)
                    ,restrict_to = 'quick_sale'
                    ,required_headers = []
                    ,optional_headers = []
                    ,values = [{<<"Event-Category">>, <<"bookkeepers">>}
                              ,{<<"Event-Name">>, <<"quick_sale_resp">>}
                              ]
                    ,types = []
                    }.

-spec update_req_definition() -> kapi_definition:api().
update_req_definition() ->
    #kapi_definition{name = <<"update_req">>
                    ,friendly_name = <<"Subscription Update Request">>
                    ,description = <<"A request to a bookkeeper to update or create a subscription">>
                    ,build_fun = fun update_req/1
                    ,validate_fun = fun update_req_v/1
                    ,publish_fun = fun publish_update_req/1
                    ,binding = ?BINDING_STRING(<<"update">>, <<"request">>)
                    ,restrict_to = 'update'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Audit">>
                                        ,<<"Invoice">>
                                        ]
                    ,optional_headers = [<<"Dry-Run">>]
                    ,values = [{<<"Event-Category">>, <<"bookkeepers">>}
                              ,{<<"Event-Name">>, <<"update_req">>}
                              ]
                    ,types = []
                    }.

-spec update_resp_definition() -> kapi_definition:api().
update_resp_definition() ->
    #kapi_definition{name = <<"update_resp">>
                    ,friendly_name = <<"Subscription Update Response">>
                    ,description = <<"The result of a subscription update request">>
                    ,build_fun = fun update_resp/1
                    ,validate_fun = fun update_resp_v/1
                    ,publish_fun = fun publish_update_resp/2
                    ,binding = ?BINDING_STRING(<<"update">>, <<"response">>)
                    ,restrict_to = 'update'
                    ,required_headers = []
                    ,optional_headers = []
                    ,values = [{<<"Event-Category">>, <<"bookkeepers">>}
                              ,{<<"Event-Name">>, <<"update_resp">>}
                              ]
                    ,types = []
                    }.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [quick_sale_req_definition()
    ,quick_sale_resp_definition()
    ,update_req_definition()
    ,update_resp_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(atom() | kz_term:text() | kz_term:ne_binary()) -> kapi_definition:api().
api_definition(Name) when is_atom(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(Name) when is_list(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"quick_sale_req">>) ->
    quick_sale_req_definition();
api_definition(<<"quick_sale_resp">>) ->
    quick_sale_resp_definition();
api_definition(<<"update_req">>) ->
    update_req_definition();
api_definition(<<"update_resp">>) ->
    update_resp_definition().

%%------------------------------------------------------------------------------
%% @doc Bind to a queue to this API exchange and events.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

-spec bind_to_q(kz_term:ne_binary(), kz_term:api_atoms()) -> 'ok'.
bind_to_q(Q, 'undefined') ->
    'ok' = kz_amqp_util:bind_q_to_bookkeepers(Q, <<"bookkeepers.*.*">>);
bind_to_q(Q, [RestrictTo|T]) ->
    try [kapi_definition:binding(Definition)
         || Definition <- api_definitions(),
            kapi_definition:restrict_to(Definition) =:= RestrictTo
        ]
    of
        [Binding] ->
            'ok' = kz_amqp_util:bind_q_to_bookkeepers(Q, Binding),
            bind_to_q(Q, T);
        _Else ->
            bind_to_q(Q, T)
    catch
        error:undef ->
            bind_to_q(Q, T)
    end;
bind_to_q(_Q, []) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Unbind from a queue of this API exchange and events.
%% @end
%%------------------------------------------------------------------------------
-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q_from(Queue, props:get_value('restrict_to', Props)).

-spec unbind_q_from(kz_term:ne_binary(), kz_term:api_atoms()) -> 'ok'.
unbind_q_from(Q, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_bookkeepers(Q, <<"bookkeepers.*.*">>);
unbind_q_from(Q, [RestrictTo|T]) ->
    try [kapi_definition:binding(Definition)
         || Definition <- api_definitions(),
            kapi_definition:restrict_to(Definition) =:= RestrictTo
        ]
    of
        [Binding] ->
            'ok' = kz_amqp_util:unbind_q_from_bookkeepers(Q, Binding),
            unbind_q_from(Q, T);
        _Else -> unbind_q_from(Q, T)
    catch
        error:undef ->
            unbind_q_from(Q, T)
    end;
unbind_q_from(_Q, []) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:bookkeepers_exchange().

%%%=============================================================================
%%% Helpers
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generic function to build API payload.
%% @end
%%------------------------------------------------------------------------------
-spec build_message(kz_term:api_terms(), kapi_definition:api()) -> api_formatter_return().
build_message(Prop, #kapi_definition{required_headers = ReqH
                                    ,optional_headers = OptH
                                    ,validate_fun = Validate
                                    ,name = _Name
                                    }) when is_list(Prop) ->
    case Validate(Prop) of
        'true' -> kz_api:build_message(Prop, ReqH, OptH);
        'false' -> {'error', "Proplist failed validation for " ++ binary_to_list(_Name)}
    end;
build_message(JObj, Definition) ->
    build_message(kz_json:to_proplist(JObj), Definition).

%%------------------------------------------------------------------------------
%% @doc Generic function to validate API payload.
%% @end
%%------------------------------------------------------------------------------
validate(Prop, #kapi_definition{required_headers = ReqH
                               ,values = Values
                               ,types = Types
                               }) when is_list(Prop) ->
    kz_api:validate(Prop, ReqH, Values, Types);
validate(JObj, Definition) ->
    validate(kz_json:to_proplist(JObj), Definition).

%%%=============================================================================
%%% Internal Bookkeepers Functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Quick Sale
%% Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec quick_sale_req(kz_term:api_terms()) -> api_formatter_return().
quick_sale_req(Prop) ->
    build_message(Prop, quick_sale_req_definition()).

-spec quick_sale_req_v(kz_term:api_terms()) -> boolean().
quick_sale_req_v(Prop) ->
    validate(Prop, quick_sale_req_definition()).

-spec publish_quick_sale_req(kz_term:api_terms()) -> 'ok'.
publish_quick_sale_req(JObj) ->
    publish_quick_sale_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_quick_sale_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_quick_sale_req(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = quick_sale_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun quick_sale_req/1),
    kz_amqp_util:bookkeepers_publish(Binding, Payload, ContentType).

-spec quick_sale_resp(kz_term:api_terms()) -> api_formatter_return().
quick_sale_resp(Prop) ->
    build_message(Prop, quick_sale_resp_definition()).

-spec quick_sale_resp_v(kz_term:api_terms()) -> boolean().
quick_sale_resp_v(Prop) ->
    validate(Prop, quick_sale_resp_definition()).

-spec publish_quick_sale_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_quick_sale_resp(RespQ, JObj) ->
    publish_quick_sale_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_quick_sale_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_quick_sale_resp(RespQ, API, ContentType) ->
    #kapi_definition{values = Values} = quick_sale_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun quick_sale_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Subscription Update
%% Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec update_req(kz_term:api_terms()) -> api_formatter_return().
update_req(Prop) ->
    build_message(Prop, update_req_definition()).

-spec update_req_v(kz_term:api_terms()) -> boolean().
update_req_v(Prop) ->
    validate(Prop, update_req_definition()).

-spec publish_update_req(kz_term:api_terms()) -> 'ok'.
publish_update_req(JObj) ->
    publish_update_req(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_update_req(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_update_req(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = update_req_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun update_req/1),
    kz_amqp_util:bookkeepers_publish(Binding, Payload, ContentType).

-spec update_resp(kz_term:api_terms()) -> api_formatter_return().
update_resp(Prop) ->
    build_message(Prop, update_resp_definition()).

-spec update_resp_v(kz_term:api_terms()) -> boolean().
update_resp_v(Prop) ->
    validate(Prop, update_resp_definition()).

-spec publish_update_resp(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_update_resp(RespQ, JObj) ->
    publish_update_resp(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_update_resp(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_update_resp(RespQ, API, ContentType) ->
    #kapi_definition{values = Values} = update_resp_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun update_resp/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).


