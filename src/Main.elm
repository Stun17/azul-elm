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
init = ({ ppocket = [], pstack = 200, pdeal = True,  pstatus = Id
        , kpocket = [], kstack = 200, kdeal = False, kstatus = Id
        , pot = 0
        , board = []
        } , Cmd.none)

subscriptions : Struc -> Sub Msg
subscriptions m = Sub.none

type alias Cards = List (Int , Int)

type alias Struc =
    { ppocket  : Cards
    , pstack   : Int
    , pdeal    : Bool
    , pstatus  : Status
    , kpocket  : Cards
    , kstack   : Int
    , kdeal    : Bool
    , kstatus  : Status
    , pot      : Int
    , board    : Cards
    }

type Status = Ca | Ch | Ra | Fo | Al | Be | Id

type Msg = Shuffle | Start (List Int) |  AllIn  


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
        in ({ s | ppocket = List.take 2 zs
                , kpocket = List.take 2 (List.drop 2 zs)
                , board   = List.take 5 (List.drop 4 zs)
                , pdeal   = s.kdeal
                , kdeal   = not s.pdeal
                , pot     = 0
             } , Cmd.none)
  AllIn ->
        if s.pstack > s.kstack
        then ( { s | pot    = s.pot + s.pstack
               ,     pstack = s.pstack - s.kstack
               ,     pstatus = Al
               ,     kstatus = Al
               } , Cmd.none)
         else ({ s | pot    = s.pot + s.pstack
               ,     pstack = 0
               ,     pstatus = Al
               ,     kstatus = Al
               } , Cmd.none)
 

----- View


view : Struc -> Html Msg
view m =
   body [ style [("font-family","mono") , ("background","green")] ]
        [ p [] [] , br [] [] ,
          div [style [("margin-left", "200px")]]
              [tabls m , p [] [] , buttns ]]


hfun2 : (Int,Int) -> Html a
hfun2 (r , s) =
  let ty z x y = td [style [("background","white")]]
        [span [style [("font-family","mono"),("color",z),("font-size","32pt")]] [text (y ++ x)]]
  in let s2 = case r of
       2  -> "2"
       3  -> "3"
       4  -> "4"
       5  -> "5"
       6  -> "6"
       7  -> "7"
       8  -> "8"
       9  -> "9"
       10 -> "10"
       11 -> "J"
       12 -> "Q"
       13 -> "K"
       14 -> "A"
       _  -> ""
     in case s of
       1 -> ty "black" "♠" s2
       2 -> ty "black" "♣" s2
       3 -> ty "red"   "♦" s2
       4 -> ty "red"   "♥" s2
       _ -> ty "green" "" ""


tabls m =
  let aaa = [("height","110px")]
      bbb = [("font-size","20pt"),("color","yellow"),("align","left"),("width","20%") ]
  in table [style [("width","45%")]]
           [ tr [style [("height","110px")]] (List.append
                [ td [style bbb] [text (toString m.kstack)]] (List.map hfun2 m.kpocket))
           , tr [style aaa ]
                [ td []
                     [img [src (if m.kdeal then "img/tycoonn.png" else "img/green.png"),
                                 height 120] [] ]
                , td [style bbb] [ case m.kstatus of
                                         Fo ->  text "Fold"
                                         Ca ->  text "Call"
                                         Ra ->  text "Check"
                                         _  ->  text "Ukn" ]]
           , tr [style aaa] (List.append [td [style bbb] [text (toString m.pot )] ]
                      (List.map hfun2 m.board) )
           , tr [style aaa]
                [ td []
                     [img [src (if m.pdeal then "img/tycoonn.png" else "img/green.png"),
                                height 120] []]
                , td [style bbb] [ case m.pstatus of
                                         Fo ->  text "Fold"
                                         Ca ->  text "Call"
                                         Ra ->  text "Check"
                                         _  ->  text "Ukn" ]]
           , tr [style aaa] (List.append [td [style bbb] [text (toString m.pstack)]]
                         (List.map hfun2 m.ppocket) ) ]


buttns =
   let bstyle = [("width","70px"),("margin-left","100px")]
   in  div [style [("background","yellow")]]
         [ button [  style bstyle]  [ text " Fold  " ]
         , button [  style bstyle]  [ text " Check " ]
         , button [  style bstyle]  [ text " Call " ]
         , button [  style bstyle]  [ text " Bet   " ]
         , button [  style bstyle]  [ text " Raise " ]
         , input  [  style [("width","100px")]  ]  [ text "100"]
         , button [ onClick AllIn , style bstyle] [ text " All-In " ]
         , button [ onClick Shuffle , style bstyle] [ text " Deal " ]
         , br [] []
         ]

