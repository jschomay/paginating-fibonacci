module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Stream exposing (Stream)
import Paginate.Custom as Paginate exposing (..)


main : Program Never (Paginated (Stream Int)) Msg
main =
    beginnerProgram { model = paginatedFib, view = view, update = update }


type Msg
    = Next
    | Prev


fib : Stream Int
fib =
    Stream.fibonocci


paginatedFib : Paginated (Stream Int)
paginatedFib =
    Paginate.init length 5 fib


length : Stream Int -> Int
length =
    always 1.0e10 >> round


slice : Int -> Int -> Stream Int -> Stream Int
slice from to stream =
    stream
        |> Stream.nextN from
        |> Tuple.first
        |> Stream.limit (to - from)


fibView : Paginated (Stream Int) -> Html Msg
fibView model =
    model
        |> Paginate.page slice
        |> Stream.toList
        |> List.map toString
        |> \s ->
            h3 [] [ text <| String.join ", " s ]


update : Msg -> Paginated (Stream Int) -> Paginated (Stream Int)
update msg model =
    case msg of
        Next ->
            next model

        Prev ->
            prev model


view : Paginated (Stream Int) -> Html Msg
view model =
    div
        [ style
            [ ( "fontFamily", "'IM Fell English', serif" )
            , ( "backgroundColor", "#f5edd6" )
            , ( "minHeight", "100vh" )
            , ( "textAlign", "center" )
            , ( "paddingTop", "3em" )
            ]
        ]
        [ node "link" [ rel "stylesheet", href "//cdn.rawgit.com/necolas/normalize.css/master/normalize.css" ] []
        , node "link" [ rel "stylesheet", href "//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css" ] []
        , node "link" [ rel "stylesheet", href "https://fonts.googleapis.com/css?family=IM+Fell+English:400,400i" ] []
        , h1 [] [ text "Paginating Fibonacci" ]
        , h4 []
            [ text "Applying "
            , a [ href "http://package.elm-lang.org/packages/jschomay/elm-paginate/latest" ] [ text "pagination" ]
            , text " to "
            , a [ href "http://package.elm-lang.org/packages/naddeoa/stream/latest" ] [ text "infinite streams" ]
            , text " in Elm!"
            ]
        , div []
            [ span [] [ text "Page " ]
            , b [] [ text <| toString <| currentPage model ]
            , span [] [ text " of " ]
            , b [] [ text "∞" ]
            ]
        , div []
            [ span [] [ text "(" ]
            , em [] [ text <| (++) " fib " <| toString <| (currentPage model - 1) * 5 ]
            , span [] [ text " - " ]
            , em [] [ text <| (++) " fib " <| toString <| (currentPage model - 1) * 5 + 4 ]
            , span [] [ text ") *" ]
            ]
        , fibView model
        , button [ onClick Prev, class "button-clear" ] [ text "Prev" ]
        , button [ onClick Next, class "button-clear" ] [ text "Next" ]
        , p [ style [ ( "fontSize", "70%" ) ] ] [ text "* Mostly, javascript looses a little precision after fib 75 or so..." ]
        , p []
            [ text "As simple as "
            , code []
                [ text <| "paginate fibonacci |> next |> page |> toList" ]
            , text "."
            ]
        , p []
            [ text "See the "
            , a [ href "https://github.com/jschomay/paginating-fibonacci/blob/master/Main.elm" ] [ text "full code" ]
            , text "."
            ]
        ]
