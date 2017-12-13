module View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)

view : Struc -> Html Msg
view m =
   body [ style [("font-family", "monospace"), ("background", "green")] ]
        [ div [style [("background", "yellow")]]
              [ p [style [("color","blue"),("font-size","12pt")]]
                  [text "demo v0.00 , MMXVII  =-= Texas-Holdem , NL200 , 1$/2$ , Heads-Up =-="] ]
        , p [] [] , br [] []
        , div [style [("margin-left", "60px")]] [ tabls m , p [] [] ]
        , p [style [("margin-top", "30px")]] [text " "]
        , buttns m
        ]

rdeck = "img/tycoonr.png"
ndeck = "img/tycoonn.png"
cardr = "img/rtycoon.png"
cardn = "img/ntycoon.png"

tabls : Struc -> Html a
tabls m = table [style [("width", "920px")]]
           [ tigrok m False
           , tdeal  m False
           , tboard m
           , tdeal  m True
           , tigrok m True
           ]
        
tigrok : Struc -> Bool -> Html a        
tigrok m f =  -- f флаг отличающий игрока от бота
    tr [style [("text-align", "center")] ]
       (List.append
         [td [style [ ("font-size", "18pt")
                    , ("color",     "yellow")
                    , ("width" ,    "80px")
                    , ("height",    "110px")
                    ] , colspan 2
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
             Sd ->
                  List.map vfun0 m.kpocket
             _ ->
                 [ td [] [img [ src (if m.deck then cardn else cardr), height 110, width 80 ] [] ]
                 , td [] [img [ src (if m.deck then cardn else cardr), height 110, width 80 ] [] ]
                 ]
         )
       )

tdeal : Struc -> Bool -> Html a       
tdeal m f =  -- f флаг отличающий игрока от бота
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
            , td [style [("font-size","16pt")
                        ,("color","yellow")
                        ,("width","80px")
                        ,("height","110px")
                        ]]
                 [ text (if f then script m.pstatus else script m.kstatus) ] ]

tboard : Struc -> Html a      
tboard m =
  tr [style [("text-align","center")]]
      (List.append 
        [ td [style [("font-size","18pt")
                    ,("color","yellow")
                    ,("width","80px")
                    ,("height","110px")
                    ], colspan 2]
          [text ("pot $" ++ (toString m.pot))]
        ]
          ( if m.gstage == St || m.gstage == Dc || m.gstage == Pr
            then List.map vfun0 [(0,0),(0,0),(0,0),(0,0),(0,0)]   
            else List.map vfun0 m.board
          )
      ) 

vfun0 : (Int,Int) -> Html a
vfun0 (s,r) =
  let ty b z x y =
          td [style [ ("valign","top")
                    , ("background",b)
                    , ("height","110px")
                    , ("width","80px")
                    ]
             ]
             [span [ style [ ("font-family","monospace")
                           , ("color",z)
                           , ("font-size","24pt")
                           ]
                   ]
                   [ text (y ++ x) ]
             ]
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
   let bstyle = [("width","80px"),("margin-left","30px")]
       cstyle = [("width","30px")]
       dstyle = [("width","50px"),("margin-left","50px")]
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
                  , Html.Attributes.min (toString m.bet)
                  , Html.Attributes.max (toString m.pstack)
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
                  , disabled (not <| List.member m.gstage [Dc,Dd,Sd])
                  , style dstyle
                  ] [ text " Deal " ]
         , button [ onClick Start
                  , style dstyle
                  , disabled (m.gstage /= St)
                  ] [ text " Start " ]

         , span [style dstyle] [text "|"]
         , span [style dstyle] [text (toString m.timer)]

         , span [style dstyle] [text "|"]
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

