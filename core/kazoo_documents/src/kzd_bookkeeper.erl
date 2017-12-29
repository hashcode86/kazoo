%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_bookkeeper).

-export([new/0]).
-export([type/0
        ,type/1
        ,set_type/1
        ]).
-export([name/1
        ,name/2
        ,set_name/2
        ]).
-export([bookkeeper/1
        ,bookkeeper/2
        ,set_bookkeeper/2
        ]).
-export([mappings/1
        ,mappings/2
        ,set_mappings/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(NAME, <<"name">>).
-define(BOOKKEEPER, <<"bookkeeper">>).
-define(MAPPINGS, <<"mappings">>).

-define(PVT_TYPE, <<"bookkeeper">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json:new(), type()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec type(kz_json:object()) -> kz_term:ne_binary().
type(JObj) ->
    kz_doc:type(JObj, type()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_type(doc()) -> doc().
set_type(JObj) ->
    kz_doc:set_type(JObj, type()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec name(doc()) -> kz_term:api_binary().
name(JObj) ->
    name(JObj, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(JObj, Default) ->
    kz_json:get_value(?NAME, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_name(doc(), kz_term:api_binary()) -> doc().
set_name(JObj, Name) ->
    kz_json:set_value(?NAME, Name, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper(doc()) -> kz_term:api_binary().
bookkeeper(JObj) ->
    bookkeeper(JObj, 'undefined').

-spec bookkeeper(doc(), Default) -> kz_term:ne_binary() | Default.
bookkeeper(JObj, Default) ->
    kz_json:get_value(?BOOKKEEPER, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper(doc(), kz_term:api_binary()) -> doc().
set_bookkeeper(JObj, Bookkeeper) ->
    kz_json:set_value(?BOOKKEEPER, Bookkeeper, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec mappings(doc()) -> kz_json:object().
mappings(JObj) ->
    mappings(JObj, kz_json:new()).

-spec mappings(doc(), Default) -> kz_json:object() | Default.
mappings(JObj, Default) ->
    kz_json:get_json_value(?MAPPINGS, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_mappings(doc(), kz_json:object()) -> kz_json:object().
set_mappings(JObj, Mappings) ->
    kz_json:set_value(?MAPPINGS, Mappings, JObj).
