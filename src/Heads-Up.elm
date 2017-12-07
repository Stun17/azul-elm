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

type Msg = Shuffle | Face (List Int) | Fold

type alias Deck = List (Int , (Int,Int))

myhashSuit = Dict.fromList [(1,"♠"),(2,"♣"),(3,"♦"),(4,"♥")]
myhashRank = Dict.fromList
             [ (2,"2"), (3,"3"), (4,"4"), (5,"5"), (6,"6"), (7,"7"), (8,"8")
             , (9,"9"), (10,"T"), (11,"J"), (12,"Q"), (13,"K"), (14,"A")
             ]
myhash52 =
  Dict.fromList
    [ (2 ,(4, 2)),(3 ,(4, 3)),(4 ,(4, 4)),(5 ,(4, 5)),(6 ,(4, 6)),(7 ,(4,7)),(8 ,(4,8)),(9 ,(4,9))
    , (10,(4,10)),(11,(4,11)),(12,(4,12)),(13,(4,13)),(14,(4,14))
    , (15,(3, 2)),(16,(3, 3)),(17,(3, 4)),(18,(3, 5)),(19,(3, 6)),(20,(3,7)),(21,(3,8)),(22,(3,9))
    , (23,(3,10)),(24,(3,11)),(25,(3,12)),(26,(3,13)),(27,(3,14))
    , (28,(2, 2)),(29,(2, 3)),(30,(2, 4)),(31,(2, 5)),(32,(2, 6)),(33,(2,7)),(34,(2,8)),(35,(2,9))
    , (36,(2,10)),(37,(2,11)),(38,(2,12)),(39,(2,13)),(40,(2,14))
    , (41,(1, 2)),(42,(1, 3)),(43,(1, 4)),(44,(1, 5)),(45,(1, 6)),(46,(1,7)),(47,(1,8)),(48,(1,9))
    , (49,(1,10)),(50,(1,11)),(51,(1,12)),(52,(1,13)),(53,(1,14))
    ]

subscriptions : List Int -> Sub Msg
subscriptions m = Sub.none

update : Msg -> List Int -> (List Int ,  Cmd Msg)
update b m = case b of
    Shuffle ->
        (m , Random.generate Face hfun0)
    Face n  ->
        (hfun1 n , Cmd.none)
    Fold ->
        ([], Cmd.none)

view : List Int -> Html Msg
view xs =
  let x11 = List.head xs
      x12 = List.head (List.drop 1 xs)
      x21 = List.head (List.drop 2 xs)
      x22 = List.head (List.drop 3 xs)
      xf1 = List.head (List.drop 4 xs)
      xf2 = List.head (List.drop 5 xs)
      xf3 = List.head (List.drop 6 xs)
      xt  = List.head (List.drop 7 xs)
      xr  = List.head (List.drop 8 xs)
  in body [style [("font-size", "32pt"),("font-family","mono"),("background","green")]]
         [ button [ onClick Shuffle ] [ text "Shuffle" ]
         , p [] []
         , table [style [("width","90%"),("cellpadding","10%"),("text-align","left")]]
                 [    tr [style [("height","100px")]]
                          [ td [style []] []
                          , td [style [("background","white")]] [hfun2 x11]
                           , td [style [("background","white")]] [hfun2 x12]
                          ]
                    , tr [style [("rowspan","5"), ("height","100px")]] []      
                    , tr [style [("height","100px")]]
                          [ td [style [("background","white")]] [hfun2 xf1]
                           , td [style [("background","white")]] [hfun2 xf2]
                           , td [style [("background","white")]] [hfun2 xf3]
                           , td [style [("background","white")]] [hfun2 xt ]
                          , td [style [("background","white")]] [hfun2 xr ]
                          ]
                    , tr [style [("rowspan","5"), ("height","100px")]] []          
                    , tr [style [("height","100px")]]
                          [ td [style []] []
                            , td [style [("background","white")]] [hfun2 x21]
                            , td [style [("background","white")]] [hfun2 x22]
                            ]
                    ]
         , button [onClick Fold ] [ text "Fold" ]
         , p [] [text "demo"]                     
         ]


hfun0 : Random.Generator (List Int)
hfun0 = Random.list 2048 (Random.int 2 53)

hfun1 : List Int -> List Int
hfun1 xs = List.Extra.unique xs

hfun2 : Maybe Int -> Html a
hfun2 k0 =
  case k0 of
    Nothing -> ty "white" "" ""
    Just k1 -> case (Dict.get k1 myhash52) of
       Just (k2,k3)  -> case (Dict.get k2 myhashSuit) of
         Just suit   ->   case (Dict.get k3 myhashRank) of
           Just rank ->     case k2 of
                              1 -> ty "black" rank suit
                              2 -> ty "black" rank suit
                              3 -> ty "red"   rank suit
                              4 -> ty "red"   rank suit
                              _ -> ty "white" "" ""
           Nothing   ->            ty "white" "" ""
         Nothing     ->            ty "white" "" ""
       Nothing       ->            ty "white" "" ""

ty c r s = span [style [("color", c)]] [text (r ++ s)]
