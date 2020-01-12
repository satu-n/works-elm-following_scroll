module Main exposing (main)

import Browser
import Browser.Dom as Dom exposing (blur, getViewportOf, setViewportOf)
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList, id)
import Json.Decode as Decode exposing (string)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Index =
    Int


type alias Task =
    { bar : String
    }


type alias Model =
    { indicator : Index
    , tasks : List Task
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { indicator = 0
      , tasks =
            [ Task "press J"
            , Task "q"
            , Task "u"
            , Task "i"
            , Task "c"
            , Task "k"
            , Task "b"
            , Task "r"
            , Task "o"
            , Task "w"
            , Task "n"
            , Task "f"
            , Task "o"
            , Task "x"
            , Task "j"
            , Task "u"
            , Task "m"
            , Task "p"
            , Task "s"
            , Task "o"
            , Task "v"
            , Task "e"
            , Task "r"
            , Task "t"
            , Task "h"
            , Task "e"
            , Task "l"
            , Task "a"
            , Task "z"
            , Task "y"
            , Task "d"
            , Task "o"
            , Task "g"
            , Task "press K"
            ]
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | CharacterKey Char
    | ControlKey String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CharacterKey 'k' ->
            ( { model
                | indicator =
                    if model.indicator > 0 then
                        model.indicator - 1

                    else
                        model.indicator
              }
            , followUp model 40
            )

        CharacterKey 'j' ->
            ( { model
                | indicator =
                    if model.indicator < List.length model.tasks - 1 then
                        model.indicator + 1

                    else
                        model.indicator
              }
            , followDown model 40
            )

        CharacterKey _ ->
            ( model, Cmd.none )

        ControlKey _ ->
            ( model, Cmd.none )


followUp : Model -> Float -> Cmd Msg
followUp m tH =
    let
        indPosY =
            tH * toFloat m.indicator
    in
    Dom.getViewportOf "tasks"
        |> Task.andThen
            (\info ->
                if indPosY < info.viewport.y + tH then
                    Dom.setViewportOf "tasks" 0 (indPosY - (info.viewport.height / 2))

                else
                    Dom.blur ""
            )
        |> Task.attempt (\_ -> NoOp)


followDown : Model -> Float -> Cmd Msg
followDown m tH =
    let
        indPosY =
            tH * toFloat m.indicator
    in
    Dom.getViewportOf "tasks"
        |> Task.andThen
            (\info ->
                if info.viewport.y + info.viewport.height - 3 * tH < indPosY then
                    Dom.setViewportOf "tasks" 0 (indPosY - (info.viewport.height / 2) + 2 * tH)

                else
                    Dom.blur ""
            )
        |> Task.attempt (\_ -> NoOp)



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ div [ id "header" ] []
        , div [ id "body" ]
            [ div [ id "lSideBar" ] []
            , div [ id "mainContainer" ]
                [ div [ id "mainHeader" ] []
                , div [ id "mainBody" ]
                    [ div [ id "taskHeader" ]
                        [ div [ class "indicator" ] []
                        , div [ class "bar" ] []
                        , div [ class "scroll" ] []
                        ]
                    , div [ id "tasks" ]
                        (List.map
                            (viewTask model)
                            (List.indexedMap Tuple.pair model.tasks)
                        )
                    ]
                ]
            , div [ id "rSideBar" ] []
            ]
        , div [ id "footer" ] []
        ]


viewTask : Model -> ( Index, Task ) -> Html Msg
viewTask m ( i, task ) =
    div
        [ classList
            [ ( "task", True )
            , ( "focused ", i == m.indicator )
            ]
        ]
        [ div [ class "indicator" ] []
        , div [ class "bar" ] [ text task.bar ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyPress keyDecoder
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toMsg (Decode.field "key" Decode.string)


toMsg : String -> Msg
toMsg keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            ControlKey keyValue
