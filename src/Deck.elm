import Html exposing (..)
import Html.Events exposing (..)
import Random

main = Html.program
    { init            = ( [(0 , 0)] , Cmd.none)
    , view            = view
    , update          = update
    , subscriptions   = subscriptions
    }

type Msg = Shuffle | Face (List (Int , Int))


subscriptions : List (Int , Int) -> Sub Msg
subscriptions m = Sub.none

    
update : Msg -> List (Int , Int) -> (List (Int , Int) ,  Cmd Msg)
update b m = case b of
    Shuffle ->
        (m , Random.generate Face myfunc)
    Face n  ->
        (n , Cmd.none)

view : List (Int , Int) -> Html Msg
view xs  =
  div []
    [ p [] (List.map ( \x -> text (toString x))  xs)
    , button [ onClick Shuffle ] [ text "Shuffle" ]
    ]

myfunc : Random.Generator (List (Int , Int))
myfunc = Random.list 52 (Random.pair (Random.int 1 4) (Random.int 1 13))

         
