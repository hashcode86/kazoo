%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_invoice).

-export([bookkeeper_id/1
        ,set_bookkeeper_id/2
        ]).
-export([items/1
        ,set_items/2
        ]).
-export([activation_charges/1
        ,set_activation_charges/2
        ]).
-export([plan_jobj/1]).

-export([empty/0]).
-export([setters/1
        ,setters/2
        ]).
-export([public_json/1]).

-export([create/3]).
-export([has_changes/1]).

-include("services.hrl").

-record(invoice, {bookkeeper_id :: kz_term:api_binary()
                 ,items = kz_services_items:empty() :: kz_services_items:items()
                 ,activation_charges = []
                 ,plan_jobj = kz_json:new() :: kz_json:object()
                 }
       ).

-opaque invoice() :: #invoice{}.
-type setter_fun() :: {fun((invoice(), Value) -> invoice()), Value}.
-type setter_funs() :: [setter_fun()].
-export_type([invoice/0
             ,setter_fun/0
             ,setter_funs/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_id(invoice()) -> kz_term:api_binary().
bookkeeper_id(#invoice{bookkeeper_id=BookkeeperId}) ->
    BookkeeperId.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_id(invoice(), kz_term:api_binary()) -> invoice().
set_bookkeeper_id(Invoice, BookkeeperId) ->
    Invoice#invoice{bookkeeper_id=BookkeeperId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec items(invoice()) -> kz_services_items:items().
items(#invoice{items=Items}) ->
    Items.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_items(invoice(), kz_services_items:items()) -> invoice().
set_items(Invoice, Items) ->
    Invoice#invoice{items=Items}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec activation_charges(invoice()) -> any().
activation_charges(#invoice{activation_charges=ActivationCharges}) ->
    ActivationCharges.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_activation_charges(invoice(), any()) -> invoice().
set_activation_charges(Invoice, ActivationCharges) ->
    Invoice#invoice{activation_charges=ActivationCharges}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec plan_jobj(invoice()) -> kz_json:object().
plan_jobj(#invoice{plan_jobj=PlanJObj}) ->
    PlanJObj.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_plan_jobj(invoice(), kz_services_plan:plan()) -> invoice().
set_plan_jobj(Invoice, PlanJObj) ->
    Invoice#invoice{plan_jobj=PlanJObj}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> invoice().
empty() ->
    #invoice{}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(setter_funs()) -> invoice().
setters(Routines) ->
    setters(empty(), Routines).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec setters(invoice(), setter_funs()) -> invoice().
setters(Invoice, Routines) ->
    lists:foldl(fun({Setter, Value}, I) ->
                        Setter(I, Value)
                end
               ,Invoice
               ,Routines
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(invoice()) -> kz_json:object().
public_json(Invoice) ->
    %% TODO: calculate totals (taxes, activations)...
    ItemsJObjs = kz_services_items:public_json(items(Invoice)),
    Props = [{<<"items">>, ItemsJObjs}
            ,{<<"activation_charges">>, []}
            ,{<<"taxes">>, []}
            ,{<<"summary">>, summary(Invoice, ItemsJObjs)}
            ,{<<"plan">>, kzd_service_plan:plan(plan_jobj(Invoice))}
            ],
    kz_json:from_list(Props).

-spec summary(invoice(), kz_json:objects()) -> kz_json:object().
summary(_Invoice, ItemsJObjs) ->
    kz_json:from_list(
      [{<<"today">>, 0.0}
      ,{<<"recurring">>, sum_item_totals(ItemsJObjs)}
      ]
     ).

-spec sum_item_totals(kz_json:objects()) -> non_neg_integer().
sum_item_totals(ItemsJObjs) ->
    sum_item_totals(ItemsJObjs, 0).

-spec sum_item_totals(kz_json:objects(), non_neg_integer()) -> non_neg_integer().
sum_item_totals([], Total) -> Total;
sum_item_totals([ItemJObj|ItemsJObjs], Total) ->
    ItemTotal = kz_json:get_float_value(<<"total">>, ItemJObj, 0),
    sum_item_totals(ItemsJObjs, ItemTotal + Total).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create(kz_services:services(), kz_term:ne_binary(), kz_services_plans:plans_list()) -> invoice().
create(Services, BookkeeperId, PlansJObj) ->
    lager:debug("generating invoice for bookkeeper ~s", [BookkeeperId]),
    Items = kz_services_items:create(Services, PlansJObj),
    Setters = [{fun set_bookkeeper_id/2, BookkeeperId}
              ,{fun set_items/2, Items}
              ,{fun set_plan_jobj/2, PlansJObj}
              ],
    kz_services_invoice:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec has_changes(invoice()) -> boolean().
has_changes(#invoice{items=Items}) ->
    kz_services_items:has_changes(Items).
