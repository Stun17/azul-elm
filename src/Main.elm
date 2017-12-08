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
init = ({ roll_c = 200
        , roll_p = 200
        , pot = 0
        , stake = 2
        , bblind = True 
        , deck = []} , Cmd.none)
       
subscriptions : Struc -> Sub Msg
subscriptions m = Sub.none

type alias Struc =
    { roll_c : Int
    , roll_p : Int
    , pot    : Int
    , stake  : Int
    , bblind : Bool              
    , deck   : List Int
    }
    
type Msg = Shuffle | Start (List Int) | Bet | AllIn | Call | Raise | Fold
                  
update : Msg -> Struc -> (Struc , Cmd Msg)
update b m = case b of
    AllIn ->
        if m.roll_p > m.roll_c
        then ({ m | pot    = m.pot + m.roll_c
              ,     roll_p = m.roll_p - m.roll_c
              } , Cmd.none)
        else ({ m | pot    = m.pot + m.roll_p
              ,     roll_p = 0
              } , Cmd.none)
    Raise ->
      ({m | pot    = m.pot + (2 * m.stake)
         ,    roll_p = m.roll_p - (2 * m.stake)
         } , Cmd.none)  
    Bet ->
        ({m | pot    = m.pot + (2 * m.stake)
         ,    roll_p = m.roll_p - (2 * m.stake)
         } , Cmd.none)
    Call ->
        ({m | pot    = m.pot + m.stake
         ,    roll_p = m.roll_p - m.stake
         } , Cmd.none)       
    Shuffle ->
        (m , Random.generate Start (Random.list 2048 (Random.int 2 53)))
    Start xs ->
        ({ roll_c = 200
         , stake  = 2
         , bblind = False
         , roll_p = 200
         , pot    = 0
         , deck   = List.Extra.unique xs
         } , Cmd.none)
    Fold ->
        ({ m | bblind = not m.bblind } , Cmd.none)
            

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
      bstyle = [("width","70px") , ("margin-left","100px")]
      mstyle = [("font-size","26pt") , ("color","yellow"), ("align","left")
               , ("width","20%")
               ]
   in body
         [ style [("font-family","mono") , ("background","green")] ]
         [ p [] [] , br [] []
         , div [style [("margin-left", "400px")]]
             [    
               table [style [("width","55%")]]
                 [    tr [ style [("height","110px")]]
                         (List.append
                              [td [style mstyle] [text (toString m.roll_c )]]
                              ( List.map hfun2 [ x11 , x12 ] ))
                    , tr [ style [("rowspan","4"), ("height","100px")]]
                           [ if (not m.bblind)
                            then img [src "img/tycoonn.png", height 120] []
                            else img [src "img/green.png", height 120] []
                          ] 
                    , tr [ style [("height","110px")]]
                         (List.append
                              [ td [style mstyle] [text (toString m.pot )] ]
                              ( List.map hfun2 [ xf1 , xf2 , xf3 , xt , xr ] ) )
                    , tr [ style [("rowspan","4"), ("height","100px")]]
                          [ if m.bblind
                            then img [src "img/tycoonn.png", height 120] []
                            else img [src "img/green.png", height 120] []
                          ]
                    , tr [ style [("height","110px")]]
                         ( List.append
                               [ td [style mstyle] [text (toString m.roll_p )] ]
                               ( List.map hfun2 [ x21 , x22 ] ))
                 ]
              ]
         , p [] []
         , div [style [("background","yellow")]] 
         [ button [  style bstyle]  [ text " Fold  " ] 
         , button [  style bstyle]  [ text " Check " ]
         , button [ onClick Call , style bstyle]  [ text " Call " ]              
         , button [ onClick Bet ,  style bstyle]  [ text " Bet   " ] 
         , button [  style bstyle]  [ text " Raise " ] 
         , input  [  style [("width","100px")]  ]  [ text "100"]      
         , button [ onClick AllIn , style bstyle] [ text " All-In " ]
         , button [ onClick Shuffle , style bstyle] [ text " Deal " ]
         , br [] []    
         ]
      ]
       
hfun2 : Maybe Int -> Html a
hfun2 k0 =
  let ty c r s = td [ style [("background","white")]]
                    [ span  [style [("font-family","mono"), ("color",c), ("font-size","32pt")]]
                            [text (s ++ r)]
                    ]
  in case k0 of
        Nothing          -> ty "green" "" ""
        Just k1          -> case (Dict.get k1 myhash52) of
           Just (k2,k3)  ->   case (Dict.get k2 myhashSuit) of
             Just suit   ->     case (Dict.get k3 myhashRank) of
               Just rank ->       case k2 of
                                    1 -> ty "black" rank suit
                                    2 -> ty "black" rank suit
                                    3 -> ty "red"   rank suit
                                    4 -> ty "red"   rank suit
                                    _ -> ty "green" "" ""
               Nothing   ->              ty "green" "" ""
             Nothing     ->              ty "green" "" ""
           Nothing       ->              ty "green" "" ""
           
