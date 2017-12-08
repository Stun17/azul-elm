import Mydeck exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Html.Attributes exposing (style, src, height, align)
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


subscriptions : List Int -> Sub Msg
subscriptions m = Sub.none

update : Msg -> List Int -> (List Int ,  Cmd Msg)
update b m = case b of
    Shuffle ->
        (m , Random.generate Face hfun0)
    Face n  ->
        (hfun1 n , Cmd.none)
    Fold ->
        (m, Cmd.none)

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
  in
      body
         [ style [("font-family","mono") , ("background","green")] ]
         [ br [] []
         , button [ onClick Shuffle  , style bstyle] [ text " Deal " ]
         , p [] [] , hr [] [] , br [] []
         , div [style [("margin-left", "400px")]]
             [    
               table [style [("width","45%")]]
                 [    tr [ style [("height","120px")]]
                         ( List.map hfun2 [ x11 , x12 ] )
        -- , tr [] [td [] [img [src "img/tycoonr.png", height 90, align "right"][]]]
                    , tr [ style [("rowspan","5"), ("height","100px")]] []
                    , tr [ style [("height","120px")]]
                         ( List.map hfun2 [ xf1 , xf2 , xf3 , xt , xr ] )
                    , tr [ style [("rowspan","5"), ("height","100px")]] []          
                    , tr [ style [("height","120px")]]
                         ( List.map hfun2 [ x21 , x22 ] )
                 ]
             ]
         -- , 
         , p [] []  , hr [] []  , br [] []
         , button [ onClick Fold , style bstyle]  [ text " Fold  " ] 
         , button [ onClick Fold , style bstyle]  [ text " Check " ] 
         , button [ onClick Fold , style bstyle]  [ text " Bet   " ] 
         , button [ onClick Fold , style bstyle]  [ text " Raise " ] 
         , input  [ style [("width","100px")]  ]  [ text "100"]      
         , button [ onClick Fold  , style bstyle] [ text " All-In " ]
         , hr [] []    
         , p [] [text "demo ver 0.0"]
         , p [] []    
         ]

bstyle = [("width","70px"), ("margin-left","100px")]          

hfun0 : Random.Generator (List Int)
hfun0 = Random.list 2048 (Random.int 2 53)

hfun1 : List Int -> List Int
hfun1 xs = List.Extra.unique xs

hfun2 : Maybe Int -> Html a
hfun2 k0 =
  let ty c r s = td
                   [ style  [ ("background","white") ] ]
                   [ span   [ style  [ ("font-family","mono")
                                     , ("color", c)  
                                     , ("font-size", "32pt")
                                     ]
                             ] 
                             [ text (s ++ r) ] 
                   ]
  in
      case k0 of
        Nothing          -> ty "white" "" ""
        Just k1          -> case (Dict.get k1 myhash52) of
           Just (k2,k3)  ->   case (Dict.get k2 myhashSuit) of
             Just suit   ->     case (Dict.get k3 myhashRank) of
               Just rank ->       case k2 of
                                    1 -> ty "black" rank suit
                                    2 -> ty "black" rank suit
                                    3 -> ty "red"   rank suit
                                    4 -> ty "red"   rank suit
                                    _ -> ty "white" "" ""
               Nothing   ->              ty "white" "" ""
             Nothing     ->              ty "white" "" ""
           Nothing       ->              ty "white" "" ""
