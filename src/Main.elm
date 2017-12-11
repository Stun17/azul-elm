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

subscriptions : Struc -> Sub Msg
subscriptions m = Sub.none

init : (Struc, Cmd msg)
init = ({ ppocket = [(0,0), (0,0)], pstack = 200, pstatus = Id
        , kpocket = [(0,0), (0,0)], kstack = 200, kstatus = Id
        , pot = 0, board = [(0,0), (0,0), (0,0), (0,0), (0,0)]
        , hand = 0, order = False, bet = 3, gstage = St, pdeal = False
        } , Cmd.none)


-----  Model

--               call, check, fold, all-in, bet, idle, win, think
type PStatus  =  Ca | Ch | Fo | Al | Be | Id | Wi | Th

--               start, choose dealer, preflop, flop, turn, river, showdown , do deal  
type GStage   =  St | Dc | Pr | Fl | Tn | Rv | Sd | Dd 

type alias Struc =
    { ppocket  : List (Int , Int)
    , kpocket  : List (Int , Int )
    , board    : List (Int , Int)                 
    , pstack   : Int
    , kstack   : Int
    , pot      : Int
    , pdeal    : Bool      -- who is SB (and dealer) now ?
    , order    : Bool      -- who is thinking now : True - player, False - krupie ?
    , pstatus  : PStatus   -- решение игрока на раздаче (на каждой улице ) и состояние
    , kstatus  : PStatus
    , gstage   : GStage    -- сосотояние розыгрыша: префлоп флоп торн ривер старт жребий вскрытие
    , hand     : Int
    , bet      : Int
    }


----- Update
    
type Msg = Start
         | Shuffle
         | Initial (List Int)
         | Hand    (List Int)
         | AllIn
         | Bet
         | Call
         | Check
         | Fold

update : Msg -> Struc -> (Struc , Cmd Msg)
update b s = case b of
  Start ->                                                  -- готовим ряд для формирования колоды
        (s, Random.generate Initial (Random.list 2048 (Random.int 2 53)))
  Initial ys ->                                             -- решаем, кто первый диллер ?
        let zs = mfun0 ys
        in case (List.take 1 zs) of
             [(s1,r1)] -> case (List.take 1 (List.drop 1 zs)) of
                [(s2,r2)] -> ( { s |
                                 pdeal    = r1 > r2 
                               , ppocket  = [(s1,r1)] , kpocket = [(s2,r2)]
                               , pstatus  = Id        , kstatus = Id
                               , gstage   = Dc        , order = False
                               } , Cmd.none)
                _ -> (s, Cmd.none)
             _ -> (s , Cmd.none)
  Shuffle ->
        (s , Random.generate Hand (Random.list 2048 (Random.int 2 53)))
  Hand ys ->                                                              -- начало каждой раздачи
        let zs = mfun0 ys                                                 -- формируем колоду
        in ( { s |
               ppocket = List.take 2 zs                                   -- формируем руки
             , kpocket = List.take 2 (List.drop 2 zs)
             , board   = List.take 5 (List.drop 4 zs)                     -- формируем боард
                         
             , pstatus = if s.pdeal then Th else Id
             , kstatus = if s.pdeal then Id else Th

             , pdeal   = if s.hand > 0 then not s.pdeal else s.pdeal
                         
             , pot     = 3                                                -- анте
             , pstack  = if s.pdeal then s.pstack - 1 else s.pstack - 2   -- анте
             , kstack  = if s.pdeal then s.pstack - 2 else s.kstack - 1   -- анте
             , hand    = s.hand + 1                                       -- счетчик раздач

             , order   = s.pdeal            
             , gstage  = Pr
             } , Cmd.none)
  Fold ->  ( { s |
               kstack  = s.pot + s.kstack
             , pot     = 0
             , pstatus = Fo
             , kstatus = Wi
             , order   = False
             , gstage  = Dd
             , pdeal   = not s.pdeal 
             }, Cmd.none)
  Check -> ( { s |
               pstatus = Ch
             , kstatus = Th
             , order   = False
             }, Cmd.none)
  Call ->  (s, Cmd.none)
  Bet  ->  (s, Cmd.none)
  AllIn ->
        if s.pstack > s.kstack
        then ( { s |
                 pot     = s.pot + s.pstack
               , pstack  = s.pstack - s.kstack
               , pstatus = Al
               , kstatus = Th
               } , Cmd.none)
         else ({ s |
                 pot     = s.pot + s.pstack
               , pstack  = 0
               , pstatus = Al
               , kstatus = Th
               , order   = False            
               } , Cmd.none)

-- функция формирования колоды из ряда случайных целых чисел и хеша,
-- где ключ - целое число , а значение - кортеж целых чисел масть/ранг
mfun0 : List Int -> List (Int , Int)
mfun0 ys =
    List.map (\k -> case (Dict.get k myhash52) of
      Just (s, r) -> (s, r)
      Nothing     -> (0, 0) )
     (List.Extra.unique ys)


----- View

view : Struc -> Html Msg
view m =
   body [ style [("font-family", "monospace")
                , ("background", "green")]
        ]
        [ div [style [("background", "yellow")]]
              [ p [style [("color","blue"),("font-size","12pt")]] [text "demo v0.00, MMXVII"] ]
        , p [] [] , br [] []
        , div [style [("margin-left", "200px")]]
            [ tabls m
            , p [] []
            ]
        , p [style [("margin-top", "20px")]] [text " "]
        , buttns m
        ]

tigrok m f =  -- f флаг отличающий игрока от крупье
    tr [style [("text-align", "center")] ]
       (List.append
         [td [style [("font-size", "22pt")
                    ,("color",     "yellow")
                    ,("width" ,    "110px")
                    ], colspan 2
             ]
             [text ("stack $" ++ (toString (if f then m.pstack else m.kstack)))]
         ]
         (if f
          then List.map vfun0 m.ppocket
          else case m.gstage of
                 Rv ->
                     [ td [] [img [ src "img/ntycoon.png", height 110, width 80 ] [] ]
                     , td [] [img [ src "img/ntycoon.png", height 110, width 80 ] [] ]
                     ]
                 Tn ->
                     [ td [] [img [ src "img/ntycoon.png", height 110, width 80 ] [] ]
                     , td [] [img [ src "img/ntycoon.png", height 110, width 80 ] [] ]
                     ]
                 Fl ->
                     [ td [] [img [ src "img/ntycoon.png", height 110, width 80 ] [] ]
                     , td [] [img [ src "img/ntycoon.png", height 110, width 80 ] [] ]
                     ]                   
                 Pr ->
                     [ td [] [img [ src "img/ntycoon.png", height 110, width 80 ] [] ]
                     , td [] [img [ src "img/ntycoon.png", height 110, width 80 ] [] ]
                     ]
                 St ->
                     [ td [] [img [ src "img/green.png",   height 110, width 80 ] [] ] 
                     , td [] [img [ src "img/green.png",   height 110, width 80 ] [] ]
                     ]
                 _  ->
                     List.map vfun0 m.kpocket
         )
       )
             
tdeal m f =  -- f флаг отличающий игрока от крупье
   let pict =
         if m.gstage == St || m.gstage == Dc
         then "img/green.png" 
         else
           case f of
               True ->
                   if m.pdeal
                   then "img/tycoonn.png"
                   else "img/green.png"
               False ->
                   if not m.pdeal
                   then "img/tycoonn.png"
                   else "img/green.png"
       script x = case x of
         Fo ->  "Fold"
         Ch ->  "Check"
         Ca ->  "Call"
         Be ->  "Bet"
         Al ->  "All in"
         Wi ->  "Winner"       
         Th ->  "..."                          
         _  ->  " "
   in tr [] [ td [] [img [ src pict, height 110, width 80 ] [] ]
            , td [style [("font-size","22pt") ,("color","yellow") ,("width" ,  "110px") ] ]
                 [ text (if f then script m.pstatus else script m.kstatus) ] ]
       
tboard m =
  tr [style [("text-align","center")]]
     [ td [style [("font-size","22pt"),("color","yellow"),("width" ,  "120px")], colspan 2]
          [text ("pot $" ++ (toString m.pot))]  
--     (case m.status of
--             St ->
                   , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
                   , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
                   , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
                   , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
                   , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]]
         --     Dc ->
         --           , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
         --           , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
         --           , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
         --           , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
         --           , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
         --     Fl ->
         --           , vfun0 (List.take 1 m.board)
         --           , vfun0 (List.take 1 (List.drop 1 m.board))
         --           , vfun0 (List.take 1 (List.drop 2 m.board))
         --           , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
         --           , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
         --     Tn ->
         --           , vfun0 (List.take 1 m.board)
         --           , vfun0 (List.take 1 (List.drop 1 m.board))
         --           , vfun0 (List.take 1 (List.drop 2 m.board))
         --           , vfun0 (List.take 1 (List.drop 3 m.board))
         --           , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
         --     Rv ->
         --           , vfun0 (List.take 1 m.board)
         --           , vfun0 (List.take 1 (List.drop 1 m.board))
         --           , vfun0 (List.take 1 (List.drop 2 m.board))
         --           , vfun0 (List.take 1 (List.drop 3 m.board))
         --           , vfun0 (List.take 1 (List.drop 4 m.board))
         --     _  ->
         --           td [] []
         -- )

tabls : Struc -> Html a
tabls m = table [style [("width", "920px")]]
           [ tigrok m False
           , tdeal  m False 
           , tboard m
           , tdeal  m True
           , tigrok m True
           ]

vfun0 : (Int,Int) -> Html a
vfun0 (s,r) =
  let ty b z x y =
          td [style [("valign","top")
                    , ("background",b)
                    , ("height","110px")
                    , ("width","80px")]
             ]
             [span [style [("font-family","monospace")
                          , ("color",z)
                          , ("font-size","24pt")]
                   ]
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

buttns : Struc -> Html Msg
buttns m =
   let bstyle = [("width","70px"),("margin-left","30px")]
       cstyle = [("width","50px")]
       dstyle = [("width","70px"),("margin-left","150px")]
   in  div [style [("background","yellow")]]
         [ button [ onClick AllIn
                  , style bstyle
                  , disabled (m.kstatus == Th || m.gstage == Dd || m.gstage == St)
                  ] [ text " All In " ]

         , button [ onClick Bet
                  , style bstyle
                  , disabled (m.kstatus == Th || m.gstage == Dd  || m.gstage == St)
                  ]  [ text " Bet " ]
         , input  [ style cstyle
                  , disabled (m.kstatus == Th || m.gstage == Dd || m.gstage == St)
                  , maxlength 3
                  , value (toString (2 * m.bet))
                  ] [ ]
         , select [ style cstyle
                  , disabled (m.kstatus == Th || m.gstage == Dd || m.gstage == St)
                  ]  -- , Input Raise ]
             [ option [value (toString (3 * m.bet))] [text (toString (3 * m.bet))]
             , option [value (toString (4 * m.bet))] [text (toString (4 * m.bet))]
             , option [value (toString (5 * m.bet))] [text (toString (5 * m.bet))]
             , option [value (toString (6 * m.bet))] [text (toString (6 * m.bet))]
             , option [value (toString (7 * m.bet))] [text (toString (7 * m.bet))]
             , option [value (toString (8 * m.bet))] [text (toString (8 * m.bet))]
             ]

         , button [ onClick Call
                  , disabled (m.kstatus == Th || m.gstage == Dd || m.gstage == St)
                  , style bstyle
                  ]  [ text " Call " ]
         , button [ onClick Check
                  , disabled (m.kstatus == Th || m.gstage == Dd || m.gstage == St)
                  , style bstyle
                  ]  [ text " Check " ]
         , button [ onClick Fold
                  , disabled (m.kstatus == Th || m.gstage == Dd || m.gstage == St)
                  , style bstyle
                  ]  [ text " Fold  " ]

         , button [ onClick Shuffle
                  , disabled (m.gstage /= Dc && m.gstage /= Dd || m.gstage == St)
                  , style dstyle
                  ] [ text " Deal " ]
         , button [ onClick Start
                  , style bstyle
                  , disabled (m.gstage /= St)
                  ] [ text " Start " ]
         , br [] []
         ]

