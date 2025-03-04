%%%-------------------------------------------------------------------
%%% @doc
%%% `embryo' - Library for managing Emergence configuration and embryo objects
%%%
%%% This module provides functions for:
%%% - Creating and manipulating embryo objects
%%% - Reading Emergence configuration files
%%% - Retrieving discovery service URLs
%%% - Merging lists of embryos
%%%
%%% @author YourName
%%% @copyright 2025 YourCompany
%%% @version 0.1.0
%%% @end
%%%-------------------------------------------------------------------
-module(embryo).

%% Public API
-export([new/1, to_map/1, merge_lists_by_url/2, read_emergence_conf/0, get_em_disco_url/0]).

%% Type specifications
-record(embryo, {properties}).
-type embryo() :: #embryo{}.
-type properties() :: map().
-type config_map() :: map().

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new embryo object with the given properties.
%%
%% @param Properties A map of properties for the embryo
%% @return A new embryo record
%% @end
%%--------------------------------------------------------------------
-spec new(properties()) -> embryo().
new(Properties) ->
    #embryo{properties = Properties}.

%%--------------------------------------------------------------------
%% @doc Converts an embryo record to a map representation.
%%
%% @param Embryo The embryo record to convert
%% @return A map representation of the embryo
%% @end
%%--------------------------------------------------------------------
-spec to_map(embryo()) -> map().
to_map(#embryo{properties = Properties}) ->
    #{properties => Properties}.

%%--------------------------------------------------------------------
%% @doc Reads the Emergence configuration file.
%%
%% Looks for emergence.conf in platform-specific locations:
%% - Unix: ~/.config/emergence/emergence.conf
%% - Windows: %APPDATA%\emergence\emergence.conf or %HOME%\AppData\Roaming\emergence\emergence.conf
%%
%% @return Configuration map or 'undefined' if the file doesn't exist
%% @end
%%--------------------------------------------------------------------
-spec read_emergence_conf() -> config_map() | undefined.
read_emergence_conf() ->
    ConfigPath = case os:getenv("HOME") of
        false ->
            case os:getenv("APPDATA") of
                false -> undefined;
                AppData -> filename:join([AppData, "emergence", "emergence.conf"])
            end;
        Home ->
            case os:type() of
                {unix, _} -> filename:join([Home, ".config", "emergence", "emergence.conf"]);
                {win32, _} -> filename:join([Home, "AppData", "Roaming", "emergence", "emergence.conf"]);
                _ -> undefined
            end
    end,

    case ConfigPath of
        undefined -> undefined;
        Path ->
            case filelib:is_file(Path) of
                true -> read_file(Path);
                false -> undefined
            end
    end.

%%--------------------------------------------------------------------
%% @doc Gets the Emergence discovery service URL.
%%
%% Tries to get the URL from:
%% 1. Environment variable "server_url"
%% 2. "em_disco/server_url" in the configuration file
%% 3. Falls back to "http://localhost:8080" if not found
%%
%% @return The discovery service URL as a string
%% @end
%%--------------------------------------------------------------------
-spec get_em_disco_url() -> string().
get_em_disco_url() ->
    case os:getenv("server_url") of
        false ->
            ConfigMap = case read_emergence_conf() of
                undefined -> #{};
                Map -> Map
            end,
            case maps:get("em_disco", ConfigMap, undefined) of
                undefined -> "http://localhost:8080";
                EmDisco ->
                    case maps:get("server_url", EmDisco, undefined) of
                        undefined -> "http://localhost:8080";
                        Url -> Url
                    end
            end;
        Url -> Url
    end.

%%--------------------------------------------------------------------
%% @doc Merges two lists of embryos, removing duplicates based on URL.
%%
%% @param UriList First list of embryo records
%% @param OtherList Second list of embryo records
%% @return A merged list with duplicates removed
%% @end
%%--------------------------------------------------------------------
-spec merge_lists_by_url([embryo()], [embryo()]) -> [embryo()].
merge_lists_by_url(UriList, OtherList) ->
    CombinedList = UriList ++ OtherList,

    % Collect all URLs to track duplicates
    {Result, _SeenUrls} = lists:foldl(
        fun(Embryo, {Acc, Seen}) ->
            Properties = Embryo#embryo.properties,
            case maps:get(<<"url">>, Properties, undefined) of
                undefined ->
                    {[Embryo | Acc], Seen};
                Url ->
                    case sets:is_element(Url, Seen) of
                        true ->
                            {Acc, Seen};
                        false ->
                            {[Embryo | Acc], sets:add_element(Url, Seen)}
                    end
            end
        end,
        {[], sets:new()},
        CombinedList
    ),

    lists:reverse(Result).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Reads the contents of a file into a map.
%%
%% @private
%% @param Path Path to the file
%% @return A map representation of the file contents
%% @end
%%--------------------------------------------------------------------
-spec read_file(string()) -> map().
read_file(Path) ->
    case file:open(Path, [read]) of
        {ok, Device} ->
            Result = read_lines(Device, #{}, ""),
            file:close(Device),
            Result;
        {error, _} ->
            #{}
    end.

%%--------------------------------------------------------------------
%% @doc Reads lines from a file and builds a configuration map.
%%
%% @private
%% @param Device File device to read from
%% @param Map Current configuration map
%% @param CurrentSection Current section being read
%% @return Updated configuration map
%% @end
%%--------------------------------------------------------------------
-spec read_lines(file:io_device(), map(), string()) -> map().
read_lines(Device, Map, CurrentSection) ->
    case file:read_line(Device) of
        {ok, Line} ->
            TrimmedLine = string:trim(Line),
            case TrimmedLine of
                [$[|Rest] ->
                    % Section header
                    Section = string:trim(Rest, trailing, "]"),
                    NewMap = Map#{Section => #{}},
                    read_lines(Device, NewMap, Section);
                "" ->
                    % Empty line
                    read_lines(Device, Map, CurrentSection);
                _ ->
                    % Key-value pair
                    case string:split(TrimmedLine, "=") of
                        [Key, Value] ->
                            TrimmedKey = string:trim(Key),
                            TrimmedValue = string:trim(Value, both, "\""),
                            NewMap = case maps:get(CurrentSection, Map, undefined) of
                                undefined ->
                                    Map;
                                SectionMap ->
                                    Map#{CurrentSection => SectionMap#{TrimmedKey => TrimmedValue}}
                            end,
                            read_lines(Device, NewMap, CurrentSection);
                        _ ->
                            % Not a key-value pair
                            read_lines(Device, Map, CurrentSection)
                    end
            end;
        eof ->
            Map;
        {error, _} ->
            Map
    end.
