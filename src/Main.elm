module Main exposing (..)

import Random
import List.Extra exposing (unique)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style, src, height, align)
import Dict
import Mydeck exposing (..)

--------------------------

main = Html.program
    { init            = init
    , subscriptions   = subscriptions
    , update          = update
    , view            = view
    }


-----  Model
    
init : (Struc, Cmd msg)
init = ({ player  = { pocket = [], stack = 200, deal = True,  status = Id }
        , krupier = { pocket = [], stack = 200, deal = False, status = Id }
        , pot = 0
        , board = []
        } , Cmd.none)

subscriptions : Struc -> Sub Msg
subscriptions m = Sub.none

type alias Cards = List (Int , Int)
type alias Player = { poket  : Cards
                    , stack  : Int
                    , deal   : Bool
                    , status : Status
                    }
type alias Struc =
    { player  : Player
    , krupie  : Player
    , pot     : Int
    , board   : Cards
    }

type Status = Ca | Ch | Ra | Fo | Al | Be | Id

type Msg = Shuffle | Start (List Int) | Bet | AllIn | Call | Raise | Fold | Check


----- Update
    

update : Msg -> Struc -> (Struc , Cmd Msg)
update b s = case b of
    Shuffle ->
        (s , Random.generate Start (Random.list 2048 (Random.int 2 53)))
    Start ys ->
        let xs  = List.Extra.unique ys
            zs  = List.map (\k -> case (Dict.get k myhash52) of
                                       Just (s,r) -> (s,r)
                                       Nothing    -> (0,0) ) xs
        in ({ m | m.player.poket = List.take 2 zs
                , krupie.poket = List.take 2 (List.drop 2 zs)
                , board        = List.take 5 (List.drop 4 zs)  
                , player.stack = 200
                , krupie.stack = 200
                , player.deal  = krupie.deal
                , krupie.deal  = not player.deal  
                , pot          = 0
             } , Cmd.none)
    AllIn ->
        if m.player.stack > m.krupie.stack
        then ({ m | pot    = m.pot + m.player.stack
              ,     player.stack = m.player.stack - m.krupie.stack
              ,     player.status = Al
              ,     krupie.status = Al                                    
              } , Cmd.none)
        else ({ m | pot    = m.pot + m.player.stack
              ,     player.stack = 0
              ,     player.status = Al
              ,     krupie.status = Al 
              } , Cmd.none)


----- View


view : Struc -> Html Msg
view m =
   let 
      bstyle = [("width","70px"),("margin-left","100px")]
      mstyle = [("font-size","20pt"),("color","yellow"),("align","left"),("width","20%") ]
   in body
         [ style [("font-family","mono") , ("background","green")] ]
         [ p [] [] , br [] []
         , div [style [("margin-left", "400px")]]
           [ table [style [("width","45%")]]
             [ tr [ style [("height","110px")]]
                  (List.append
                     [td [style mstyle] [text (toString m.krupie.stack)]]
                     (List.map hfun2 m.krupie.poket)
             , tr [ style [("height","100px")]]
                  [ if m.krupie.deal
                    then td [] [img [src "img/tycoonn.png", height 120] [] ]
                    else td [] [img [src "img/green.png",   height 120] [] ]
                  , case m.krupie.status of
                      Fo -> td [style mstyle] [text "Fold"]
                      Ca -> td [style mstyle] [text "Call"]
                      Ra -> td [style mstyle] [text "Check"]
                      _  -> td [style mstyle] [text "Ukn"]
                   ]
              , tr [style [("height","110px")]]
                   (List.append
                      [td [style mstyle] [text (toString m.pot )] ]
                      (List.map hfun2 m.board)
              , tr [style [("height","100px")]]
                   [ if m.player.deal
                     then td [] [img [src "img/tycoonn.png", height 120] [] ]
                     else td [] [img [src "img/green.png",   height 120] [] ]
                   , case m.stat of
                       Fo -> td [style mstyle] [text "Fold"]
                       Ca -> td [style mstyle] [text "Call"]
                       Ra -> td [style mstyle] [text "Check"]
                       _  -> td [style mstyle] [text "Ukn"]
                    ]
                , tr [style [("height","110px")]]
                     ( List.append
                         [td [style mstyle] [text (toString m.player.stack)]]
                             (List.map hfun2 m.player.poket)
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

hfun2 : (Int,Int) -> Html a
hfun2 (r , s) =
  let ty z x y = td [style [("background","white")]]
        [span [style [("font-family","mono"),("color",z),("font-size","32pt")]] [text (y ++ x)]]
  in case s of
       2  ->  s2 = "2"
       3  ->  s2 = "3"
       4  ->  s2 = "4"
       5  ->  s2 = "5"
       6  ->  s2 = "6"
       7  ->  s2 = "7"
       8  ->  s2 = "8"
       9  ->  s2 = "9"
       10 ->  s2 = "10"
       11 ->  s2 = "J"
       12 ->  s2 = "Q"
       13 ->  s2 = "K"
       14 ->  s2 = "A"
       _  ->  s2 = ""
     case r of             
       1 -> ty "black" "♠" s2
       2 -> ty "black" "♣" s2
       3 -> ty "red"   "♦" s2
       4 -> ty "red"   "♥" s2
       _ -> ty "green" "" ""


