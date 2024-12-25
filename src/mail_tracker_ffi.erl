-module(mail_tracker_ffi).

-export([system_info/0, install/1, add_item/5, get_item_by_id/1, get_item_by_to/1, get_item_by_from/1, delete_item_by_to/1, delete_item_by_id/1, list_db/0]).

-record(mail_tracker_items, {id, to, to_address, from, from_address}).

system_info() ->
    mnesia:system_info().

install(Nodes) ->
    % Stop Mnesia to install schema
    application:stop(mnesia),

    % Delete schema from all nodes
    _ = mnesia:delete_schema(Nodes),

    % Install schema on all nodes
    _ = mnesia:create_schema(Nodes),

    % Start Mnesia to create table
    application:start(mnesia),

    % Make tables
    case mnesia:create_table(mail_tracker_items, [{attributes, record_info(fields, mail_tracker_items)}, {ram_copies, Nodes}]) of
        {_, ok} -> {ok, 0};
        {aborted, {already_exists,mail_tracker_items}} -> {ok, 1};
        Error -> {error, Error}
    end.

add_item(Id, To, A0, From, A1) ->
    % Write function for insertion
    F = fun() ->
        mnesia:write(#mail_tracker_items{id=Id, to=To, to_address=A0, from=From, from_address=A1})
    end,

    % Replace result atom with tuple so it can be passed up to Gleam correctly
    case mnesia:activity(transaction, F) of
        ok -> {ok, 0};
        Error -> {error, Error}
    end.

get_item_by_from(From) ->
    % Write pattern to match agains
    Pattern = #mail_tracker_items{_ = '_', from=From},

    % Write function for query
    F = fun() ->
        Res = mnesia:match_object(Pattern),
        [{Id, To, A0, From, A1} || #mail_tracker_items{id=Id, to=To, to_address=A0, from_address=A1} <- Res]
    end,

    % Extract results and turn into gleam result
    case mnesia:activity(transaction, F) of
        [] -> {error, "no results"};
        X -> {ok, X}
    end.

get_item_by_to(To) ->
    % Write pattern to match agains
    Pattern = #mail_tracker_items{_ = '_', to=To},

    % Write function for query
    F = fun() ->
        Res = mnesia:match_object(Pattern),
        [{Id, To, A0, From, A1} || #mail_tracker_items{id=Id, to_address=A0, from=From, from_address=A1} <- Res]
    end,

    % Extract results and turn into gleam result
    case mnesia:activity(transaction, F) of
        [] -> {error, "no results"};
        X -> {ok, X}
    end.

get_item_by_id(Id) ->
    % Write pattern to match agains
    Pattern = #mail_tracker_items{_ = '_', id=Id},

    % Write function for query
    F = fun() ->
        Res = mnesia:match_object(Pattern),
        [{Id, To, A0, From, A1} || #mail_tracker_items{to=To, to_address=A0, from=From, from_address=A1} <- Res]
    end,

    % Extract results and turn into gleam result
    case mnesia:activity(transaction, F) of
        [{Id, To, A0, From, A1}] -> {ok, {Id, To, A0, From, A1}};
        Error -> {error, Error}
    end.

delete_item_by_to(To) ->
    % Write function for deletion
    F = fun() ->
        mnesia:delete({mail_tracker_items, To})
    end,
    case mnesia:activity(transaction, F) of
        ok -> {ok, 0};
        Error -> {error, Error}
    end.

delete_item_by_id(Id) ->
    % Write function for deletion
    F = fun() ->
        mnesia:delete({mail_tracker_items, Id})
    end,
    case mnesia:activity(transaction, F) of
        ok -> {ok, 0};
        Error -> {error, Error}
    end.

list_db() ->
    % Make a function to be run 
    F = fun() ->
        mnesia:all_keys(mail_tracker_items)
    end,

    % Run function as transaction
    case mnesia:activity(transaction, F) of
        {ok, Data} -> {ok, Data};
        {aborted, no_transaction} -> {error, no_transaction};
        {error, X} -> {error, X};
        X -> {ok, X}
    end.
