import Html exposing (..)
import Html.Events exposing (..)
import Random

main = Html.program
    { init            = ( (1,1) , Cmd.none)
    , view            = view
    , update          = update
    , subscriptions   = subscriptions
    }

type Msg = Shuffle | Face (Int , Int)


subscriptions : (Int , Int) -> Sub Msg
subscriptions m = Sub.none

    
update : Msg -> (Int , Int) -> ((Int , Int) ,  Cmd Msg)
update b m = case b of
    Shuffle ->
        (m , Random.generate Face (Random.pair (Random.int 1 4) (Random.int 1 13)))
    Face n  ->
        (n , Cmd.none)

view : (Int , Int) -> Html Msg
view (s,r)  =
  div []
    [ p [] [ text (toString s) ]
    , button [ onClick Shuffle ] [ text "Shuffle" ]
    ]


         
