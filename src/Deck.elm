import Html exposing (..)
import Html.Events exposing (..)
import Random
import Html.Attributes exposing (style)
import List.Extra exposing (unique)
import Dict

main = Html.program
    { init            = ([] , Cmd.none)
    , view            = view
    , update          = update
    , subscriptions   = subscriptions
    }

type Msg = Shuffle | Face (List Int)
type alias Deck = List (Int , String)
    
myhash =
    Dict.fromList
        [ (1  , "A♥") , (2  , "2♥") , (3  , "3♥") , (4  , "4♥") , (5  , "5♥") , (6  , "6♥")
        , (7  , "7♥") , (8  , "8♥") , (9  , "9♥") , (10 , "T♥") , (11 , "J♥") , (12 , "Q♥")
        , (13 , "K♥")
        , (14 , "A♦") , (15 , "2♦") , (16 , "3♦") , (17 , "4♦") , (18 , "5♦") , (19 , "6♦")
        , (20 , "7♦") , (21 , "8♦") , (22 , "9♦") , (23 , "T♦") , (24 , "J♦") , (25 , "Q♦")
        , (26 , "K♦")
        , (27 , "A♣") , (28 , "2♣") , (29 , "3♣") , (30 , "4♣") , (31 , "5♣") , (32 , "6♣")
        , (33 , "7♣") , (34 , "8♣") , (35 , "9♣") , (36 , "T♣") , (37 , "J♣") , (38 , "Q♣")
        , (39 , "K♣")
        , (40 , "A♠") , (41 , "2♠") , (42 , "3♠") , (43 , "4♠") , (44 , "5♠") , (45 , "6♠")
        , (46 , "7♠") , (47 , "8♠") , (48 , "9♠") , (49 , "T♠") , (50 , "J♠") , (51 , "Q♠")
        , (52 , "K♠")             
        ]
                            
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
hfun1 xs = List.map (\x -> let s2 =
                             case Dict.get x myhash of
                               Just s1 -> s1
                               Nothing -> " " 
                           in text (" --- " ++ s2)) xs

hfun2 : List Int -> List Int
hfun2 xs = List.Extra.unique xs

{-           
hfun3 : Int -> List (Html m)          
hfun3 xs =
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
