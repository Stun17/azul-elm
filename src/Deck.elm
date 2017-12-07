import Html exposing (..)
import Html.Events exposing (..)
import Random
import Html.Attributes exposing (style)

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
    [ span [style [("font-size", "24pt") , ("background" , "lime")]] (himfunc xs)
    , p [] []      
    , button [ onClick Shuffle ] [ text "Shuffle" ]
    ]

myfunc : Random.Generator (List (Int , Int))
myfunc = Random.list 52 (Random.pair (Random.int 1 4) (Random.int 1 13))

himfunc : List (Int , Int) -> List (Html m)          
himfunc xs =
    List.map
        ( \(s , r)  ->
          let r1 = case r of
                     1  -> "A"
                     2  -> "2"
                     3  -> "3"
                     4  -> "4"
                     5  -> "5"
                     6  -> "6"
                     7  -> "7"
                     8  -> "8"
                     9  -> "9"
                     10 -> "T"
                     11 -> "J"
                     12 -> "Q"
                     13 -> "K"
                     _  -> "Joker"      
              s1 = case s of
                     1  -> "♥"
                     2  -> "♦"
                     3  -> "♣"
                     4  -> "♠"
                     _  -> " "
          in span [] [text (" " ++ r1 ++ s1 ++ " ")]
      ) xs
