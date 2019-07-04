module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Tab as Tab
import Bootstrap.Table as Table
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Loading as Loader exposing( LoaderType(..), defaultConfig, render)



---- MODEL ----


type alias Player =
    { name : String
    , victories : Int
    }


type alias Team =
    { player1 : String
    , player2 : String
    , visitor : Bool
    }


type alias MatchResult =
    { result : String
    , teams : List Team
    }


type alias Match =
    { details : Maybe (List MatchResult)
    , division : String
    , id : String
    , players : List Player
    , time : Maybe String
    }


type alias RivalStats =
    { defeats : Int
    , points : Int
    , victories : Int
    }


type alias Rival =
    { name : String
    , stats : Maybe RivalStats
    }


type alias Participant =
    { details : List Rival
    , name : String
    , points : Int
    , position : Int
    }


type alias Division =
    { matches : List Match
    , name : String
    , participants : List Participant
    }


type FetchStatus
    = Failure
    | Loading
    | Success


type alias Model =
    { divisions : List Division
    , tabState : Tab.State
    , fetchStatus : FetchStatus
    }


init : ( Model, Cmd Msg )
init =
    ( { divisions =
            [ Division [] "Division 1" []
            , Division [] "Division 2" []
            ]
      , tabState = Tab.initialState
      , fetchStatus = Loading
      }
    , getLeaderboard
    )



---- HTTP ----


playerDecoder : Decoder Player
playerDecoder =
    map2 Player
        (field "name" string)
        (field "victories" int)


teamDecoder : Decoder Team
teamDecoder =
    map3 Team
        (field "player1" string)
        (field "player2" string)
        (field "visitor" bool)


matchResultDecoder : Decoder MatchResult
matchResultDecoder =
    map2 MatchResult
        (field "result" string)
        (list teamDecoder |> field "teams")


matchDecoder : Decoder Match
matchDecoder =
    map5 Match
        (list matchResultDecoder |> maybe |> field "details")
        (field "division" string)
        (field "id" string)
        (list playerDecoder |> field "players")
        (field "time" (nullable string))


rivalStatsDecoder : Decoder RivalStats
rivalStatsDecoder =
    map3 RivalStats
        (field "points" int)
        (field "victories" int)
        (field "defeats" int)


rivalDecoder : Decoder Rival
rivalDecoder =
    map2 Rival
        (field "name" string)
        (maybe rivalStatsDecoder |> field "stats")


participantDecoder : Decoder Participant
participantDecoder =
    map4 Participant
        (list rivalDecoder |> field "details")
        (field "name" string)
        (field "points" int)
        (field "position" int)


divisionDecoder : Decoder Division
divisionDecoder =
    map3 Division
        (list matchDecoder |> field "matches")
        (field "name" string)
        (list participantDecoder |> field "participants")


leaderboardDecoder : Decoder (List Division)
leaderboardDecoder =
    list divisionDecoder


getLeaderboard : Cmd Msg
getLeaderboard =
    Http.get
        { url = "http://www.json-generator.com/api/json/get/coNZmhVhOW?indent=2"
        , expect = Http.expectJson GotLeaderboard leaderboardDecoder
        }



---- UPDATE ----


type Msg
    = TabMsg Tab.State
    | GotLeaderboard (Result Http.Error (List Division))

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        TabMsg state ->
            ( { model | tabState = state }
            , Cmd.none
            )
        GotLeaderboard result ->
            case result of
                Ok text ->
                    ( { model | divisions = text, fetchStatus = Success }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | fetchStatus = Failure }
                    , Cmd.none
                    )



---- VIEW ----


generateSummaryTab divisions =
    Tab.item
        { id = "summary"
        , link = Tab.link [] [ text "Summary" ]
        , pane =
            Tab.pane [ Spacing.mt3 ]
                [ h4 [] [ text "Tab 1 Heading" ]
                , p [] [ text "Contents of tab 1." ]
                ]
        }


generateLeaderboardRow { name, position, points, details } =
    Table.tr []
        [ Table.td [] [ String.fromInt position |> text ]
        , Table.td [] [ text name ]
        , Table.td [] [ String.fromInt points |> text ]
        , Table.td [] [ List.length details
            |> String.fromInt
            |> text
        ]
        , Table.td [] [
            List.foldr
                (\detail total ->
                    case detail.stats of
                        Nothing ->
                            total
                        Just stats ->
                            stats.victories + stats.defeats + total
                )
                0
                details
            |> String.fromInt
            |> text
        ]
        , Table.td [] [ String.fromInt points |> text ]
        ]


generateLeaderboardTable participants =
    Table.table
        { options = [ Table.striped, Table.hover ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "Pos" ]
                , Table.th [] [ text "Player" ]
                , Table.th [] [ text "Points" ]
                , Table.th [] [ text "# Rivals" ]
                , Table.th [] [ text "# Matches" ]
                , Table.th [] [ text "Details" ]
                ]
        , tbody =
            Table.tbody [] (List.map generateLeaderboardRow participants)
        }

generatePendingMatchesTable matches = div [][]

generatePlayedMatchesRow {time, players} =
    Table.tr [] (
        Table.td [] [ text time ]
        ::
        List.concatMap
            (\{name, victories} ->
                [
                Table.td [] [ text name ]
                , Table.td [] [ String.fromInt victories |> text ]
                ]
            )
            players
        ++ [Table.td [] []]
    )



generatePlayedMatchesTable matches =
    Table.table
        { options = [ Table.striped, Table.hover ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "Time" ]
                , Table.th [] [ text "Player 1" ]
                , Table.th [] []
                , Table.th [] [ text "Player 2" ]
                , Table.th [] []
                , Table.th [] [ text "Player 3" ]
                , Table.th [] []
                , Table.th [] [ text "Player 4" ]
                , Table.th [] []
                , Table.th [] [ text "Details" ]
                ]
        , tbody =
            Table.tbody [] (
                List.map
                    generatePlayedMatchesRow
                    (List.map
                        (\match ->
                            case match.time of
                                Just (time as timeValue) ->
                                    {match | time = timeValue}
                        )
                        (List.filter
                            (\match ->
                                case match.time of
                                    Nothing ->
                                        False
                                    Just time ->
                                        True
                            )
                            matches
                        )
                    )
            )
        }



generateDivisionTab division =
    Tab.item
        { id = division.name
        , link = Tab.link [] [ text division.name ]
        , pane =
            Tab.pane [ Spacing.mt3 ]
                [ generateLeaderboardTable division.participants
                , generatePendingMatchesTable division.matches
                , h2 [] [text "Matches already played"]
                , generatePlayedMatchesTable division.matches
                ]
        }


generateTabItems divisions =
    generateSummaryTab divisions :: List.map generateDivisionTab divisions |> Tab.items


createTabs state =
    if state.fetchStatus == Loading then
        Loader.render Bars defaultConfig Loader.On
    else
        Tab.config TabMsg |> generateTabItems state.divisions |> Tab.view state.tabState


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , createTabs model
        ]


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
