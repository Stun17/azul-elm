module Main exposing (..)

import Random
import List.Extra exposing (unique)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time, second)
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
subscriptions m = Time.every 10000 Tick   -- Time.every second

init : (Struc, Cmd msg)
init = ({ ppocket = [(0,0), (0,0)], pstack = 200, pstatus = Id
        , kpocket = [(0,0), (0,0)], kstack = 200, kstatus = Id
        , pot = 0, board = [(0,0), (0,0), (0,0), (0,0), (0,0)]
        , hand = 0, order = False, bet = 1, gstage = St
        , pdeal = False, timer = 0, block = False, deck = True
        } , Cmd.none)


-----  Model

type PStatus =  Ca -- call
              | Ch -- check 
              | Fo -- fold 
              | Al -- all-in
              | Be -- bet
              | Id -- idle
              | Wi -- win
              | Th -- thinking

type GStage  =  St -- start
              | Dc -- choose dealer
              | Pr -- preflop
              | Fl -- flop
              | Tn -- turn
              | Rv -- river
              | Sd -- showdown
              | Dd -- do deal

type alias Struc =
    { ppocket  : List (Int , Int)
    , kpocket  : List (Int , Int )
    , board    : List (Int , Int)
    , pstack   : Int
    , kstack   : Int
    , pot      : Int
    , pdeal    : Bool      -- who is SB now ?
    , order    : Bool      -- who is thinking now : True - player, False - krupie ?
    , pstatus  : PStatus   -- решение игрока на раздаче (на каждой улице ) и состояние
    , kstatus  : PStatus   -- решение крупье на раздаче и его текущее состояние
    , gstage   : GStage    -- сосотояние розыгрыша: префлоп флоп торн ривер старт жребий вскрытие
    , hand     : Int       -- количество отыгранных раздач
    , bet      : Int       -- текущая ставка
    , timer    : Int
    , block    : Bool      -- to block drop-up "bet xN" after the first usage
    , deck     : Bool             
    }


----- Update

type Msg = Start
         | Deal
         | Initial    (List Int)
         | Hand       (List Int)
         | AllIn
         | Bet        String
         | Call
         | Check
         | Fold
         | ChangeBet  String
         | Tick       Time
         | SwitchToRed 
         | SwitchToBlue 

update : Msg -> Struc -> (Struc , Cmd Msg)
update b s = case b of
  SwitchToRed ->
      ({ s | deck = False }, Cmd.none)
  SwitchToBlue ->
      ({ s | deck = True }, Cmd.none)
  Start ->                                                  -- готовим ряд для формирования колоды
        (s, Random.generate Initial (Random.list 2048 (Random.int 2 53)))
  Initial ys ->                                             -- решаем, кто первый диллер ?
        let zs = mfun0 ys
        in case (List.take 1 zs) of
             [(s1,r1)] -> case (List.take 1 (List.drop 1 zs)) of
                [(s2,r2)] -> ( { s |
                                 pdeal    = r2 > r1
                               , ppocket  = [(s2,r2)] , kpocket = [(s1,r1)]
                               , pstatus  = Id        , kstatus = Id
                               , gstage   = Dc        , order = False
                               }, Cmd.none)
                _ -> (s, Cmd.none)
             _ -> (s, Cmd.none)
  Deal ->
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

             , pot     = 3 , bet = 1                                      -- анте
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
             -- , gstage  = Dd
             -- , pdeal   = not s.pdeal
             }, Cmd.none)
  Check -> ( { s |
               pstatus = Ch
             , kstatus = Th
             , order   = False
             }, Cmd.none)
  Call -> ( { s |
               pot     = s.pot + s.bet
             , pstack  = s.pstack - s.bet
             , bet     = if (s.pdeal && s.gstage == Pr) then 2 else s.bet
             , pstatus = Ca
             , kstatus = Th
             , order   = False
             } , Cmd.none)
  Bet x ->
         let z = Result.withDefault s.bet (String.toInt x)
         in ( { s |
                pot     = s.pot + z
              , pstack  = s.pstack - z
              , bet     = z
              , pstatus = Be
              , kstatus = Th
              , order   = False
              } , Cmd.none)
  ChangeBet x ->
         let y = Result.withDefault s.bet (String.toInt x)
             z =  if y > s.pstack then s.pstack else {-if y < s.bet then s.bet  else -} y
         in  ({ s | bet = z , block = True }, Cmd.none)
  AllIn ->
        if s.pstack > s.kstack
        then ( { s |
                 bet     = s.kstack     
               , pot     = s.pot + s.kstack
               , pstack  = s.pstack - s.kstack
               , pstatus = Al
               , kstatus = Th
               , order   = False
               } , Cmd.none)
         else ({ s |
                 bet     = s.pstack     
               , pot     = s.pot + s.pstack
               , pstack  = 0
               , pstatus = Al
               , kstatus = Th
               , order   = False
               } , Cmd.none)
  Tick x ->
      ( { s |
           kstatus = if s.kstatus == Wi || s.kstatus == Fo then Id else s.kstatus
        ,  pstatus = if s.pstatus == Wi || s.pstatus == Fo then Id else s.pstatus
        ,  gstage  = if s.pstatus == Wi || s.kstatus == Wi then Dd else s.gstage
        ,  timer = s.timer + 1 }, Cmd.none )


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
        , p [style [("margin-top", "30px")]] [text " "]
        , buttns m
        ]

rdeck = "img/tycoonr.png"
ndeck = "img/tycoonn.png"
cardr = "img/rtycoon.png"
cardn = "img/ntycoon.png"        
       
       
tigrok m f =  -- f флаг отличающий игрока от крупье
    tr [style [("text-align", "center")] ]
       (List.append
         [td [style [("font-size", "18pt")
                    ,("color",     "yellow")
                    ,("width" ,    "110px")
                    ], colspan 2
             ]
             [text <| "stack $" ++ (toString <| if f then m.pstack else m.kstack)]
         ]
         ( if f
           then List.map vfun0 m.ppocket
           else case m.gstage of
             St ->
                 [ td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
                 , td [] [img [ src "img/green.png", height 110, width 80 ] [] ]
                 ]
             Dc ->
                  List.map vfun0 m.kpocket
             _ ->
                 [ td [] [img [ src (if m.deck then cardn else cardr), height 110, width 80 ] [] ]
                 , td [] [img [ src (if m.deck then cardn else cardr), height 110, width 80 ] [] ]
                 ]
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
                   then (if m.deck then ndeck else rdeck)
                   else "img/green.png"
               False ->
                   if not m.pdeal
                   then (if m.deck then ndeck else rdeck)
                   else "img/green.png"
       script x = case x of
         Fo ->  "fold"
         Ch ->  "check"
         Ca ->  "call $"   ++ (toString m.bet)
         Be ->  "bet $"    ++ (toString m.bet)
         Al ->  "all in $" ++ (toString m.bet)
         Wi ->  "winner"
         Th ->  "thinking..."
         _  ->  " "
   in tr [] [ td [] [img [ src pict, height 110, width 80 ] [] ]
            , td [style [("font-size","16pt") ,("color","yellow") ,("width" ,  "110px") ] ]
                 [ text (if f then script m.pstatus else script m.kstatus) ] ]

tboard m =
  tr [style [("text-align","center")]]
     [ td [style [("font-size","18pt"),("color","yellow"),("width" ,  "120px")], colspan 2]
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
   let bstyle = [("width","110px"),("margin-left","30px")]
       cstyle = [("width","50px")]
       dstyle = [("width","70px"),("margin-left","50px")]
   in  div [style [("background","yellow")]]
         [ button [ onClick AllIn
                  , style bstyle
                  , disabled (List.member m.kstatus [Th] || List.member m.gstage [Dd,Dc,St])
                  ] [ text " All In " ]

         , button [ onClick (Bet <| toString m.bet)
                  , style bstyle
                  , disabled (List.member m.kstatus [Th] || List.member m.gstage [Dd,Dc,St])
                  ]  [ text <| " Bet " ++ (toString m.bet)]
         , input  [ style cstyle
                  , disabled (  List.member m.kstatus [Th]
                             || List.member m.gstage [Dd,Dc,St] )
                  , maxlength 3
                  , placeholder " "
                  , onInput ChangeBet
                  ] [ ]
         , select [ style cstyle
                  , disabled (   List.member m.kstatus [Th]
                              || List.member m.gstage [Dd,Dc,St]
                              || m.block)
                  , onInput ChangeBet ]
             [ option [value (toString (3 * m.bet))] [text "x3"]
             , option [value (toString (4 * m.bet))] [text "x4"]
             , option [value (toString (5 * m.bet))] [text "x5"]
             , option [value (toString (6 * m.bet))] [text "x6"]
             , option [value (toString (7 * m.bet))] [text "x7"]
             ]

         , button [ onClick Call
                  , disabled (List.member m.kstatus [Th] || List.member m.gstage [Dd,Dc,St])
                  , style bstyle
                  ]  [ text " Call " ]
         , button [ onClick Check
                  , disabled (   List.member m.kstatus [Th]
                              || m.pdeal
                              || List.member m.gstage [Dd,Dc,St])
                  , style bstyle
                  ]  [ text " Check " ]
         , button [ onClick Fold
                  , disabled (List.member m.kstatus [Th] || List.member m.gstage [Dd,Dc,St])
                  , style bstyle
                  ]  [ text " Fold  " ]

         , span [style dstyle] [text "|"]
             
         , button [ onClick Deal
                  , disabled (not <| List.member m.gstage [Dc,Dd])
                  , style dstyle
                  ] [ text " Deal " ]
         , button [ onClick Start
                  , style dstyle
                  , disabled (m.gstage /= St)
                  ] [ text " Start " ]

         , span [style dstyle] [text "|"]
         , span [style dstyle] [text (toString m.timer)]
         , span [style dstyle] []             
         , input [ type_ "radio"
                 , value "red"
                 , name "color"
                 , onClick SwitchToRed] [] , text "Red"
         , input [ type_ "radio"
                 , value "blue"
                 , name "color"
                 , onClick SwitchToBlue] [] , text "Blue" 
         , br [] []
         ]

