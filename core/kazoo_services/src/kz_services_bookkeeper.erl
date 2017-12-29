%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_bookkeeper).

-export([quick_sale/3]).
-export([maybe_update/1]).
-export([junk/3]).

-include("services.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec quick_sale(kz_term:ne_binary(), integer(), kz_term:api_binary()) -> {'ok', kz_json:object()}.
quick_sale(_AccountId, _Amount, _Reason) ->
    {'ok', kz_json:new()}.

-spec junk(_, _, _) -> _.
junk(AccountId, Amount, Reason) ->
    Services = kz_services:fetch(AccountId),
    Transaction = kz_transaction:debit(AccountId, Amount),
    Transaction1 = kz_transaction:set_reason(Reason, Transaction),
    lager:info("attempting to top up account ~s for ~p", [AccountId, Amount]),

    case kz_services_bookkeeper:charge_transactions(Services, [Transaction1]) of
        [] ->
            lager:info("account ~s top up successfully for ~p", [AccountId, Amount]),
            case kz_transaction:save(kz_transaction:set_type(<<"credit">>, Transaction1)) of
                {'ok', _} ->
                    lager:info("auto top up transaction for account ~s saved successfully", [AccountId]);
                {'error', 'conflict'} ->
                    lager:warning("did not write top up transaction for account ~s already exist for today", [AccountId]);
                {'error', _Reason} ->
                    lager:error("failed to write top up transaction ~p , for account ~s (amount: ~p)"
                               ,[_Reason, AccountId, Amount]
                               )
            end;
        [FailedTransaction] ->
            _Reason = kz_json:get_value(<<"failed_reason">>, FailedTransaction),
            lager:error("failed to top up account ~s: ~p", [AccountId, _Reason]),
            {'error', 'bookkeeper_failed'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update(kz_services:services()) -> kz_services:services().
maybe_update(Services) ->
    case kz_services_invoices:has_changes(Services) of
        'false' ->
            lager:debug("no changes to any invoices", []),
            Services;
        'true' ->
            Invoices = kz_services_invoices:changed(Services),
            _BookkeeperResults =
                kz_services_invoices:foldl(invoices_foldl_fun(Services)
                                          ,[]
                                          ,Invoices
                                          ),
            Services
    end.

-spec invoices_foldl_fun(kz_services:services()) -> kz_services:invoices_foldl().
invoices_foldl_fun(Services) ->
    fun(Invoice, Results) ->
            Request = [{<<"Account-ID">>, kz_services:account_id(Services)}
                      ,{<<"Invoice">>, kz_services_invoice:public_json(Invoice)}
                      ,{<<"Audit">>, kz_services:audit_log(Services)}
                       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                      ],
            file:write_file("/tmp/test.json", io_lib:format("~s~n", [kz_json:encode(kz_json:from_list(Request))])),
            _Result = kz_amqp_worker:call(Request
                                         ,fun kapi_bookkeepers:publish_update_req/1
                                         ,fun kapi_bookkeepers:update_resp_v/1
                                         ),
            io:format("result: ~p~n", [_Result]),
            Results
    end.
