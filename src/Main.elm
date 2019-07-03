module Main exposing (Model, Msg(..), init, main, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Browser
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Player =
    { name : String
    , victories : Int
    }


type alias Team =
    { player1 : String
    , player2 : String
    }


type alias Result =
    { teams : List Team
    , result : String
    , visitor : Bool
    }


type alias Match =
    { time : Maybe String
    , division : String
    , players : List Player
    , details : Maybe (List Result)
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


type alias Model =
    { divisions : List Division
    , tabState : Tab.State
    }


init : ( Model, Cmd Msg )
init =
    ( { divisions =
            [ Division "Division 1" [] []
            , Division "Division 2" [] []
            ]
      , tabState = Tab.initialState
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = TabMsg Tab.State


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        TabMsg state ->
            ( { model | tabState = state }
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


generateDivisionTab {name} =
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
