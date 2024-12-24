-module(mail_tracker_ffi).

-export([system_info/0, install/1, add_item/4, get_item_by_to/1]).

-record(mail_tracker_items, {to, to_address, from, from_address}).

system_info() ->
    mnesia:system_info().

install(Nodes) ->
    % Install schema on all nodes
    _ = mnesia:create_schema(Nodes),

    % Make tables
    case mnesia:create_table(mail_tracker_items, [{attributes, record_info(fields, mail_tracker_items)}, {ram_copies, Nodes}]) of
        {_, ok} -> {ok, 0};
        Error -> {error, Error}
    end.

add_item(To, A0, From, A1) ->
    % Write function for insertion
    F = fun() ->
        mnesia:write(#mail_tracker_items{to=To, to_address=A0, from=From, from_address=A1})
    end,

    % Replace result atom with tuple so it can be passed up to Gleam correctly
    case mnesia:activity(transaction, F) of
        ok -> {ok, 0};
        Error -> {error, Error}
    end.

get_item_by_to(To) ->
    % Write function for query
    F = fun() ->
        case mnesia:read({mail_tracker_items, To}) of
            % If we found it
            [#mail_tracker_items{to_address=A0, from=From, from_address=A1}] -> {To, A0, From, A1};

            % If we did not find it
            [] -> undefined
        end
    end,

    % Extract results and turn into gleam result
    case mnesia:activity(transaction, F) of
        {To, A0, From, A1} -> {ok, {To, A0, From, A1}};
        Error -> {error, Error}
    end.

