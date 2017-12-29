%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2018, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_transactions).

-export([bookkeeper/1
        ,bookkeeper_type/1
        ,bookkeeper_vendor_id/1
        ]).
-export([fetch/1
        ,fetch/2
        ,fetch/3
        ]).
-export([legacy_total/1
        ,legacy_total/3
        ]).

-include("kazoo_transactions.hrl").

-define(LIST_BY_TIMESTAMP, <<"transactions/list_by_timestamp">>).

-record(bookkeeper, {type :: kz_term:ne_binary()
                    ,vendor_id :: kz_term:api_binary()
                    }
       ).

-type bookkeeper() :: #bookkeeper{}.
-type transactions() :: [kz_transaction:transaction()].
-export_type([bookkeeper/0
             ,transactions/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper(kz_term:ne_binary()) -> {'ok', bookkeeper()} | {'error', any()}.
bookkeeper(Account) ->
    AccountId = kz_util:format_account_id(Account),
    ResellerId = kz_services_reseller:get_id(AccountId),
    case kapps_util:get_master_account_id() of
        {'error', _Resaon} = Error -> Error;
        {'ok', ResellerId} ->
            {'ok', #bookkeeper{type = <<"braintree">>
                             ,vendor_id = ResellerId
                             }
            };
        {'ok', _OtherResellerId} ->
            {'error', 'unsupported_reseller'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_type(kz_term:ne_binary() | bookkeeper()) -> kz_term:api_binary().
bookkeeper_type(?NE_BINARY=Account) ->
    case bookkeeper(Account) of
        {'error', _Reason} -> 'undefined';
        {'ok', Bookkeeper} ->
            bookkeeper_type(Bookkeeper)
    end;
bookkeeper_type(#bookkeeper{type=Type}) ->
    Type.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_vendor_id(kz_term:ne_binary() | bookkeeper()) -> kz_term:api_binary().
bookkeeper_vendor_id(?NE_BINARY=Account) ->
    case bookkeeper(Account) of
        {'error', _Reason} -> 'undefined';
        {'ok', Bookkeeper} ->
            bookkeeper_vendor_id(Bookkeeper)
    end;
bookkeeper_vendor_id(#bookkeeper{vendor_id=VendorId}) ->
    VendorId.

%%------------------------------------------------------------------------------
%% @doc fetch last transactions from From to To
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary()) -> {'ok', transactions()} | {'error', any()}.
fetch(Account) ->
    fetch(Account, []).

-spec fetch(kz_term:ne_binary(), kz_term:proplist()) -> {'ok', transactions()} | {'error', any()}.
fetch(Account, Options) ->
    ViewOptions = [{'result_key', <<"doc">>}
                  ,'include_docs'
                  ,'missing_as_error'
                   | Options
                  ],
    case kazoo_modb:get_results(Account, ?LIST_BY_TIMESTAMP, ViewOptions) of
        {'error', _}=Error -> Error;
        {'ok', JObjs} ->
            {'ok', [kz_transaction:from_json(JObj) || JObj <- JObjs]}
    end.

-spec fetch(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) ->
                   {'ok', transactions()} |
                   {'error', any()}.
fetch(Account, CreatedFrom, CreatedTo)
  when is_integer(CreatedFrom), CreatedFrom > 0,
       is_integer(CreatedTo), CreatedTo > 0 ->
    MODbs = kazoo_modb:get_range(Account, CreatedFrom, CreatedTo),
    ViewOptions = [{'databases', MODbs}
                  ,{'startkey', CreatedFrom}
                  ,{'endkey', CreatedTo}
                  ,{'result_key', <<"doc">>}
                  ,'include_docs'
                  ,'missing_as_error'
                  ],
    get_ranged(?LIST_BY_TIMESTAMP, ViewOptions).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec legacy_total(kz_term:ne_binary()) -> kz_currency:available_units_return().
legacy_total(Account) ->
    {Year, Month, _} = erlang:date(),
    legacy_total(Account, Year, Month).

-spec legacy_total(kz_term:ne_binary(),  kz_time:year(), kz_time:month()) ->
                          kz_currency:available_units_return().
legacy_total(Account, Year, Month) ->
    View = <<"transactions/legacy_total">>,
    ViewOptions = [{'year', Year}
                  ,{'month', Month}
                  ,{'group_level', 1}
                  ,{'result_key', <<"value">>}
                  ,'reduce'
                  ,'missing_as_error'
                  ],
    case kazoo_modb:get_results(Account, View, ViewOptions) of
        {'error', _Reason} = Error -> Error;
        {'ok', [Total]} -> {'ok', Total};
        {'ok', []} -> {'error', 'no_legacy_transactions'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_ranged(kz_term:ne_binary(), kz_term:proplist()) -> {'ok', transactions()} | {'error', any()}.
get_ranged(View, Options) ->
    MODbs = props:get_value('databases', Options, []),
    case MODbs =:= [] of
        'true' -> {'error', 'no_account_db'};
        'false' ->
            ViewOptions = props:delete('databases', Options),
            lager:debug("getting transactions starting from ~p to ~p from dbs: ~p"
                       ,[props:get_value('startkey', ViewOptions)
                        ,props:get_value('endkey', ViewOptions)
                        ,MODbs
                        ]),
            get_ranged(View, Options, MODbs, [])
    end.

-spec get_ranged(kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binaries(), kz_json:objects()) ->
                        {'ok', transactions()} |
                        {'error', any()}.
get_ranged(_View, _Options, [], Results) -> {'ok', Results};
get_ranged(View, Options, [MODb|MODbs], Results) ->
    case kazoo_modb:get_results(MODb, View, Options) of
        {'error', _Reason} = Error -> Error;
        {'ok', JObjs} ->
            Transactions = [kz_transaction:set_modb(
                              kz_transaction:from_json(JObj), MODb
                             )
                            || JObj <- JObjs
                           ],
            get_ranged(View, Options, MODbs, Transactions ++ Results)
    end.

