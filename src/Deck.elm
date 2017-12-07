import Html exposing (..)
import Html.Events exposing (..)
import Random
import Html.Attributes exposing (style)

main = Html.program
    { init            = ( [] , Cmd.none)
    , view            = view
    , update          = update
    , subscriptions   = subscriptions
    }

type Msg = Shuffle | Face (List Int)


subscriptions : List Int -> Sub Msg
subscriptions m = Sub.none

    
update : Msg -> List Int -> (List Int ,  Cmd Msg)
update b m = case b of
    Shuffle ->
        (m , Random.generate Face myfunc)
    Face n  ->
        (hfun2 n , Cmd.none)

view : List Int -> Html Msg
view xs  =
  div []
    [ p [style [("font-size", "24pt")]] (hfun1 xs)
    , p [] []      
    , button [ onClick Shuffle ] [ text "Shuffle" ]
    ]

myfunc : Random.Generator (List Int)
myfunc = Random.list 512 (Random.int 1 52)

hfun1 : List Int -> List (Html a)
hfun1 xs = List.map (\x -> text (" " ++ (toString x))) xs

hfun2 : List Int -> List Int
hfun2 xs = List.filter (\x -> if List.member x (Listxs then 0 else x) xs



{-                  
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
 -}
