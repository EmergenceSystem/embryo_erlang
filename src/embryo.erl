%%%-------------------------------------------------------------------
%%% @doc
%%% `embryo' - Bibliothèque pour gérer la configuration Emergence et les objets embryon
%%%
%%% Ce module fournit des fonctions pour :
%%% - Créer et manipuler des objets embryon
%%% - Lire les fichiers de configuration Emergence
%%% - Récupérer les URLs du service de découverte
%%% - Fusionner des listes d'embryons
%%%
%%% @author Steve Roques
%%% @version 0.1.1
%%% @end
%%%-------------------------------------------------------------------
-module(embryo).

%% API publique
-export([new/1, to_map/1, merge_lists_by_url/2, read_emergence_conf/0, get_em_disco_url/0]).

%% Spécifications de types
-record(embryo, {properties :: map()}).
-type embryo() :: #embryo{}.
-type config_map() :: map().

%%====================================================================
%% Fonctions API
%%====================================================================

-spec new(map()) -> embryo().
new(Properties) ->
    #embryo{properties = Properties}.

-spec to_map(embryo()) -> map().
to_map(#embryo{properties = Properties}) ->
    #{properties => Properties}.

-spec read_emergence_conf() -> config_map() | undefined.
read_emergence_conf() ->
    ConfigPath = get_config_path(),
    case ConfigPath of
        undefined -> undefined;
        Path ->
            case filelib:is_file(Path) of
                true -> read_file(Path);
                false -> undefined
            end
    end.

-spec get_em_disco_url() -> string().
get_em_disco_url() ->
    case os:getenv("server_url") of
        false ->
            ConfigMap = read_emergence_conf(),
            get_url_from_config(ConfigMap);
        Url -> Url
    end.

-spec merge_lists_by_url([embryo()], [embryo()]) -> [embryo()].
merge_lists_by_url(UriList, OtherList) ->
    CombinedList = UriList ++ OtherList,
    {Result, _} = lists:foldl(fun merge_embryo/2, {[], sets:new()}, CombinedList),
    lists:reverse(Result).

%%====================================================================
%% Fonctions internes
%%====================================================================

-spec get_config_path() -> string() | undefined.
get_config_path() ->
    case os:getenv("HOME") of
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
    end.

-spec read_file(string()) -> map().
read_file(Path) ->
    case file:read_file(Path) of
        {ok, Binary} ->
            Lines = binary:split(Binary, <<"\n">>, [global, trim_all]),
            parse_config_lines(Lines, #{}, "");
        {error, _} ->
            #{}
    end.

-spec parse_config_lines([binary()], map(), string()) -> map().
parse_config_lines([], Map, _) -> Map;
parse_config_lines([Line|Rest], Map, CurrentSection) ->
    case Line of
        <<"[", Rest1/binary>> ->
            Section = binary_to_list(binary:part(Rest1, 0, byte_size(Rest1)-1)),
            parse_config_lines(Rest, Map#{Section => #{}}, Section);
        <<>> ->
            parse_config_lines(Rest, Map, CurrentSection);
        _ ->
            case binary:split(Line, <<"=">>) of
                [Key, Value] ->
                    TrimmedKey = string:trim(binary_to_list(Key)),
                    TrimmedValue = string:trim(binary_to_list(Value)),
                    NewMap = case maps:get(CurrentSection, Map, undefined) of
                        undefined -> Map;
                        SectionMap ->
                            Map#{CurrentSection => SectionMap#{TrimmedKey => TrimmedValue}}
                    end,
                    parse_config_lines(Rest, NewMap, CurrentSection);
                _ ->
                    parse_config_lines(Rest, Map, CurrentSection)
            end
    end.

-spec get_url_from_config(map() | undefined) -> string().
get_url_from_config(undefined) -> "http://localhost:8080";
get_url_from_config(ConfigMap) ->
    case maps:get("em_disco", ConfigMap, undefined) of
        undefined -> "http://localhost:8080";
        EmDisco ->
            maps:get("server_url", EmDisco, "http://localhost:8080")
    end.

-spec merge_embryo(embryo(), {[embryo()], sets:set()}) -> {[embryo()], sets:set()}.
merge_embryo(Embryo, {Acc, Seen}) ->
    Properties = Embryo#embryo.properties,
    case maps:get(<<"url">>, Properties, undefined) of
        undefined ->
            {[Embryo | Acc], Seen};
        Url ->
            case sets:is_element(Url, Seen) of
                true -> {Acc, Seen};
                false -> {[Embryo | Acc], sets:add_element(Url, Seen)}
            end
    end.

