module Main exposing (..)

import Random
import List.Extra exposing (unique)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dict
import Mydeck exposing (..)

--------------------------

main : Program Never Struc Msg
main = Html.program
    { init            = init
    , subscriptions   = subscriptions
    , update          = update
    , view            = view
    }


-----  Model

init : (Struc, Cmd msg)
init = ({ ppocket = [(0,0)], pstack = 200, pstatus = Id, pdeal = True
        , kpocket = [(0,0)], kstack = 200, kstatus = Id
        , pot = 0, board = [(0,0)], hand = -1, order = True, bet = 3
        } , Cmd.none)

subscriptions : Struc -> Sub Msg
subscriptions m = Sub.none

type Status = Ca | Ch | Fo | Al | Be | Id | Wi
    
type alias Struc =
    { ppocket  : List (Int , Int) 
    , kpocket  : List (Int , Int )
    , pstack   : Int
    , kstack   : Int
    , pot      : Int
    , pdeal    : Bool      -- who is Small Blind and dealer now ?
    , order    : Bool      -- who is thinking now ?       
    , pstatus  : Status
    , kstatus  : Status
    , hand     : Int
    , board    : List (Int , Int)
    , bet      : Int             
    }
    
type Msg = Start
         | Shuffle
         | Initial (List Int)
         | Hand (List Int)
         | AllIn
         | Bet 
         | Call
         | Check
         | Fold  

----- Update

update : Msg -> Struc -> (Struc , Cmd Msg)
update b s = case b of
  Start ->   -- готовим ряд для формирования колоды
        (s , Random.generate Initial (Random.list 2048 (Random.int 2 53)))
  Initial ys ->                    -- решаем, кто первый диллер ?
        let zs = mfun0 ys
        in case (List.take 1 zs) of
             [(s1,r1)] -> case (List.take 1 (List.drop 1 zs)) of
                [(s2,r2)] -> if r1 > r2
                             then ( { s |
                                      pdeal   = True
                                    , hand    = s.hand + 1          
                                    , ppocket = [(s1,r1)]
                                    , kpocket = [(s2,r2)]            
                                    } , Cmd.none )
                              else if r1 < r2
                                   then  ( { s |
                                             pdeal   = False
                                           , hand    = s.hand + 1            
                                           , ppocket = [(s1,r1)]
                                           , kpocket = [(s2,r2)]                                    
                                           } , Cmd.none)
                                    else ( { s |
                                             pdeal   = True
                                           , hand    = s.hand + 1            
                                           , ppocket = [(s1,r1)]
                                           , kpocket = [(s2,r2)]                              
                                           } , Cmd.none)
                _ -> (s, Cmd.none)            
             _ -> (s , Cmd.none)
  Shuffle ->
        (s , Random.generate Hand (Random.list 2048 (Random.int 2 53)))
  Hand ys ->  -- начало розыгрыша очередной раздачи
        let zs = mfun0 ys  -- формируем колоду
        in ( { s |
               ppocket = List.take 2 zs                         -- формируем руки
             , kpocket = List.take 2 (List.drop 2 zs)
             , board   = List.take 5 (List.drop 4 zs)           -- формируем боард
             , pdeal   = if s.hand > 0 then not s.pdeal else s.pdeal
             , pot     = 3                                      -- анте
             , pstack  = if s.pdeal then s.pstack - 1 else s.pstack - 2
             , kstack  = if s.pdeal then s.pstack - 2 else s.kstack - 1
             , hand    = s.hand + 1                             -- счетчик раздач
             } , Cmd.none)
  Fold -> (s, Cmd.none)
  Check -> (s, Cmd.none)          
  Call -> (s, Cmd.none)
  Bet  -> (s, Cmd.none)
  AllIn ->
        if s.pstack > s.kstack
        then ( { s |
                 pot    = s.pot + s.pstack
               , pstack = s.pstack - s.kstack
               , pstatus = Al
               , kstatus = Al
               } , Cmd.none)
         else ({ s |
                 pot    = s.pot + s.pstack
               , pstack = 0
               , pstatus = Al
               , kstatus = Al
               } , Cmd.none)

-- функция формирования колоды из ряда случайных чисел и хеша,
-- где ключ - число , а значение - пара чисел масть/ранг
mfun0 : List Int -> List (Int , Int)
mfun0 ys = List.map (\k -> case (Dict.get k myhash52) of
                                    Just (s,r) -> (s,r)
                                    Nothing    -> (0,0) )
                    (List.Extra.unique ys)


----- View

view : Struc -> Html Msg
view m =
   body [ style [("font-family","monospace") , ("background","green")] ]
        [  div [style [("background","yellow")]]
               [ p [style [("color","blue"),("font-size","12pt")]] [text "demo v0.00, MMXVII"] ]
        ,  p  [] [] , br [] []
        , div [style [("margin-left","200px")]] [tabls m, p [] [] ]
        , p [style [("margin-top","20px")]] [text " "]    
        , buttns m
        ]

vfun0 : (Int,Int) -> Html a
vfun0 (s,r) =
  let ty b z x y =
          td [style [("valign","top") , ("background",b) , ("height","100px") , ("width","80px")]]
             [span [style [("font-family","monospace") , ("color",z) , ("font-size","28pt")]]
                          [text (y ++ x)]]
      r2 = case r of
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
          1 -> ty "white" "black" r2 "♠" 
          2 -> ty "white" "black" r2 "♣" 
          3 -> ty "white" "red"   r2 "♦" 
          4 -> ty "white" "red"   r2 "♥" 
          0 -> ty "green" "green" "" ""
          _ -> ty "green" "green" "" ""     

tabls : Struc -> Html a            
tabls m =
  let bbb = [("height", "100px"),("width" ,  "80px")]
      ccc = [("width" ,  "80px")]
      ddd = [("font-size","22pt"),("color","yellow"),("width" ,  "120px")]
      fff = [("text-align","center")]      
  in  table [style [("width", "920px")]]
           [ tr [style fff]
                 (List.append [td [style ddd, colspan 2] [text ("$" ++ (toString m.kstack))]]
                                 (List.map vfun0 m.kpocket))
           , tr [] 
                [ td [] [img [ src (if m.pdeal then "img/green.png" else "img/tycoonn.png")
                             , height 110
                             , width 110 ] [] ]
                , td [] [] , td [] [] , td [] [] , td [] [] , td [] [] , td [] []
                , td [style ddd] [ case m.kstatus of
                                         Fo ->  text "Fold"
                                         Ch ->  text "Check"
                                         Ca ->  text "Call"
                                         Be ->  text "Bet"
                                         _  ->  text "Idle" ]]
           , tr [style fff] (List.append
                 (List.append [td [style ddd] [text ("pot $" ++ (toString m.pot))]
                              ,td [style ddd] []
                              ]
                              (List.map vfun0 m.board))
                 [td [] [] , td [style ddd] [text ("hand " ++ (toString m.hand))]]  ) 
           , tr [] [ td [] [img [ src (if m.pdeal then "img/tycoonn.png" else "img/green.png")
                                , height 110
                                , width 110 ] [] ]
                , td [] [] , td [] [] , td [] [] , td [] [] , td [] [] , td [] []
                , td [style ddd] [ case m.pstatus of
                                         Fo ->  text "Fold"
                                         Ch ->  text "Check"                                       
                                         Ca ->  text "Call"
                                         Be ->  text "Bet"
                                         _  ->  text "Idle" ]]
           , tr [style fff] (List.append [td [style ddd, colspan 2]
                                             [text ("$" ++ (toString m.pstack))]]
                                 (List.map vfun0 m.ppocket) ) ]

buttns : Struc -> Html Msg
buttns m =
   let bstyle = [("width","70px"),("margin-left","30px")]
       cstyle = [("width","50px")]
       dstyle = [("width","70px"),("margin-left","150px")]                
   in  div [style [("background","yellow")]]
         [ button [ onClick AllIn   , style bstyle ] [ text " All In " ]
         , button [ onClick Bet    , style bstyle ]  [ text " Bet " ]
         , input  [ style cstyle , maxlength 3 , value (toString (2 * m.bet))] [ ]
         , select [ style cstyle  ]  -- , Input Raise ]
             [ option [value (toString (3 * m.bet))] [text (toString (3 * m.bet))]
             , option [value (toString (4 * m.bet))] [text (toString (4 * m.bet))]
             , option [value (toString (5 * m.bet))] [text (toString (5 * m.bet))]
             , option [value (toString (6 * m.bet))] [text (toString (6 * m.bet))]
             , option [value (toString (7 * m.bet))] [text (toString (7 * m.bet))]
             , option [value (toString (8 * m.bet))] [text (toString (8 * m.bet))]                 
             ]
         , button [ onClick Call   , style bstyle ]  [ text " Call " ]
         , button [ onClick Check  , style bstyle ]  [ text " Check " ]
         , button [ onClick Fold   , style bstyle ]  [ text " Fold  " ]
         , button [ onClick Shuffle , style dstyle ] [ text " Deal " ]
         , button [ onClick Start   , style bstyle , disabled (m.hand >= 0) ] [ text " Start " ]
         , br [] []
         ]

