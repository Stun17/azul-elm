module Main exposing (..)

import Random
import List.Extra exposing (unique)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style, src, height, align)
import Dict
import Mydeck exposing (..)

main = Html.program
    { init            = init 
    , subscriptions   = subscriptions
    , update          = update
    , view            = view
    }

init : (Struc, Cmd msg)    
init = ({ roll_c = 200 , roll_p = 200 , deck = []} , Cmd.none)
       
subscriptions : Struc -> Sub Msg
subscriptions m = Sub.none

type alias Struc = { roll_c : Int , roll_p : Int , deck : List Int }
    
type Msg = Shuffle | Show (List Int)
                  
update : Msg -> Struc -> (Struc , Cmd Msg)
update b m =
  let hfun0 = Random.list 2048 (Random.int 2 53)
  in case b of
    Shuffle ->
        (m , Random.generate Show hfun0)
    Show m ->
        ({ roll_c = 200 , roll_p = 200 ,  deck = List.Extra.unique m } , Cmd.none)

view : Struc -> Html Msg
view m =
  let xs = m.deck
      x11 = List.head xs
      x12 = List.head (List.drop 1 xs)
      x21 = List.head (List.drop 2 xs)
      x22 = List.head (List.drop 3 xs)
      xf1 = List.head (List.drop 4 xs)
      xf2 = List.head (List.drop 5 xs)
      xf3 = List.head (List.drop 6 xs)
      xt  = List.head (List.drop 7 xs)
      xr  = List.head (List.drop 8 xs)
      bstyle = [("width","70px"), ("margin-left","100px")]          
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
                    , tr [ style [("rowspan","5"), ("height","100px")]] []
                    , tr [ style [("height","120px")]]
                         ( List.map hfun2 [ xf1 , xf2 , xf3 , xt , xr ] )
                    , tr [ style [("rowspan","5"), ("height","100px")]] []          
                    , tr [ style [("height","120px")]]
                         ( List.map hfun2 [ x21 , x22 ] )
                 ]
             ]

         , p [] []  , hr [] []  , br [] []
         , button [  style bstyle]  [ text " Fold  " ] 
         , button [  style bstyle]  [ text " Check " ] 
         , button [  style bstyle]  [ text " Bet   " ] 
         , button [  style bstyle]  [ text " Raise " ] 
         , input  [  style [("width","100px")]  ]  [ text "100"]      
         , button [  style bstyle] [ text " All-In " ]

         , br [] [] , hr [] []    
         , p [] [text "demo ver 0.0"]
         , p [style [("margin","30")]] [text "MMXVII"]    
         ]


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
           
