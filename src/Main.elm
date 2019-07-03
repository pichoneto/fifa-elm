module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)



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
    { teams : List Team
    , result : String
    }


type alias Match =
    { time : Maybe String
    , division : String
    , players : List Player
    , details : Maybe (List MatchResult)
    , id : Int
    }


type alias RivalStats =
    { points : Int
    , victories : Int
    , defeats : Int
    }


type alias Rival =
    { name : String
    , stats : Maybe RivalStats
    }


type alias Participant =
    { name : String
    , position : Int
    , points : Int
    , details : List Rival
    }


type alias Division =
    { name : String
    , participants : List Participant
    , matches : List Match
    }


type FetchStatus
    = Failure
    | Loading
    | Success String


type alias Model =
    { divisions : List Division
    , tabState : Tab.State
    , fetchStatus : FetchStatus
    }


init : ( Model, Cmd Msg )
init =
    ( { divisions =
            [ Division "Division 1" [] []
            , Division "Division 2" [] []
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
        (field "teams" (list teamDecoder))
        (field "result" string)


matchDecoder : Decoder Match
matchDecoder =
    map5 Match
        (field "time" (nullable string))
        (field "division" string)
        (field "players" (list playerDecoder))
        (field "details" (nullable (list matchResultDecoder)))
        (field "id" int)


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
        (field "stats" (nullable rivalStatsDecoder))


participantDecoder : Decoder Participant
participantDecoder =
    map4 Participant
        (field "name" string)
        (field "position" int)
        (field "points" int)
        (field "details" (list rivalDecoder))


divisionDecoder : Decoder Division
divisionDecoder =
    map3 Division
        (field "name" string)
        (field "participants" (list participantDecoder))
        (field "matches" (list matchDecoder))


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
                    ( { model | divisions = text }
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


generateDivisionTab { name } =
    Tab.item
        { id = name
        , link = Tab.link [] [ text name ]
        , pane =
            Tab.pane [ Spacing.mt3 ]
                [ h4 [] [ text "Tab 2 Heading" ]
                , p [] [ text "This is something completely different." ]
                ]
        }


generateTabItems divisions =
    generateSummaryTab divisions :: List.map generateDivisionTab divisions |> Tab.items


createTabs state =
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
