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
        , pot = 0, board = [(0,0)], hand = 0, order = True
        } , Cmd.none)

subscriptions : Struc -> Sub Msg
subscriptions m = Sub.none

type Status = Ca | Ch | Ra | Fo | Al | Be | Id
    
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
    }
    
type Msg = Start | Shuffle | Initial (List Int) | Hand (List Int) |  AllIn  | Bet 

----- Update

update : Msg -> Struc -> (Struc , Cmd Msg)
update b s = case b of
  Start ->
        (s , Random.generate Initial (Random.list 2048 (Random.int 2 53)))
  Initial ys ->
        let zs = mfun0 ys
        in case (List.take 1 zs) of
                [(s1,r1)] -> 
                     case (List.take 1 (List.drop 1 zs)) of
                         [(s2,r2)] ->
                            if r1 > r2
                            then ( { s |
                                     pdeal = True
                                   , ppocket = [(s1,r1)]
                                   , kpocket = [(s2,r2)]            
                                   } , Cmd.none )
                            else if r1 < r2
                                 then ( { s |
                                          pdeal = False
                                        , ppocket = [(s1,r1)]
                                        , kpocket = [(s2,r2)]                                    
                                        } , Cmd.none)
                                 else ( { s |
                                          pdeal = True
                                        , ppocket = [(s1,r1)]
                                        , kpocket = [(s2,r2)]                              
                                        } , Cmd.none)
                         _ -> (s, Cmd.none)            
                _ -> (s , Cmd.none)
  Shuffle ->
        (s , Random.generate Hand (Random.list 2048 (Random.int 2 53)))
  Hand ys ->
        let zs = mfun0 ys
        in ( { s |
               ppocket = List.take 2 zs
             , kpocket = List.take 2 (List.drop 2 zs)
             , board   = List.take 5 (List.drop 4 zs)
             , pdeal   = if s.hand > 0 then not s.pdeal else s.pdeal
             , pot     = 3
             , hand    = s.hand + 1
             , pstack  = if s.pdeal then s.pstack - 1 else s.pstack - 2
             , kstack  = if s.pdeal then s.pstack - 2 else s.kstack - 1
             } , Cmd.none)
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
  Bet ->
      (s, Cmd.none)

             
mfun0 : List Int -> List (Int , Int)
mfun0 ys = List.map (\k -> case (Dict.get k myhash52) of
                                    Just (s,r) -> (s,r)
                                    Nothing    -> (0,0) )
                    (List.Extra.unique ys)

----- View


view : Struc -> Html Msg
view m =
   body [ style [("font-family","monospace") , ("background","green")] ]
        [ p  [] [] , br [] []
        , div [style [("margin-left","200px")]] [tabls m, p [] [] ]
        , buttns m
        ]

vfun0 : (Int,Int) -> Html a
vfun0 (s,r) =
  let ty b z x y = td [style [ ("valign","top")
                             , ("background",b)
                             , ("height","100px")
                             , ("width","80px")]
                      ]
                      [span [style [ ("font-family","monospace")
                                   , ("color",z)
                                   , ("font-size","28pt")
                                   ]
                            ]
                            [text (y ++ x)]]
  in let r2 = case r of
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
      ddd = [("font-size","28pt"),("color","yellow")]
      fff = [("text-align","center")]      
  in  table [style [("width", "780px")]]
           [ tr [style fff]
                 (List.append [td [style ddd] [text ("$" ++ (toString m.kstack))]]
                                 (List.map vfun0 m.kpocket))
           , tr [] 
                [ td [] [img [ src (if m.pdeal then "img/green.png" else "img/tycoonn.png")
                             , height 110
                             , width 110 ] [] ]
                , td [] [] , td [] [] , td [] [] , td [] [] , td [] []
                , td [style ddd] [ case m.kstatus of
                                         Fo ->  text "Fold"
                                         Ca ->  text "Call"
                                         Ra ->  text "Check"
                                         _  ->  text "Idle" ]]
           , tr [style fff] (List.append
                 (List.append [td [style ddd] [text ("pot $" ++ (toString m.pot))]]
                              (List.map vfun0 m.board))
                 [td [] [] , td [style ddd] [text ("hand " ++ (toString m.hand))]]  ) 
           , tr [] [ td [] [img [ src (if m.pdeal then "img/tycoonn.png" else "img/green.png")
                                , height 110
                                , width 110 ] [] ]
                , td [] [] , td [] [] , td [] [] , td [] [] , td [] []
                , td [style ddd] [ case m.pstatus of
                                         Fo ->  text "Fold"
                                         Ca ->  text "Call"
                                         Ra ->  text "Check"
                                         _  ->  text "Idle" ]]
           , tr [style fff] (List.append [td [style ddd] [text ("$" ++ (toString m.pstack))]]
                                 (List.map vfun0 m.ppocket) ) ]

buttns : Struc -> Html Msg
buttns m =
   let bstyle = [("width","70px"),("margin-left","30px")]
       cstyle = [("width","50px")]
       dstyle = [("width","70px"),("margin-left","150px")]                
   in  div [style [("background","yellow")]]
         [ button [ style bstyle ]  [ text " Fold  " ]
         , button [ style bstyle ]  [ text " Check " ]
         , button [ style bstyle ]  [ text " Call " ]
         , button [ onClick Bet , style bstyle ]  [ text " Bet " ]
         , input  [ style cstyle , maxlength 3 , value "4"] [ ]
         , select [ style cstyle  ]  -- , Input Raise ]
             [ option [value "6"] [text "6"]
             , option [value "8"] [text "8"]
             , option [value "10"] [text "10"]
             , option [value "20"] [text "20"]
             , option [value "50"] [text "50"]
             , option [value "100"] [text "100"]                 
             ]
         , button [ onClick AllIn   , style bstyle ] [ text " All-In " ]
         , button [ onClick Shuffle , style dstyle ] [ text " Deal " ]
         , button [ onClick Start   , style bstyle, disabled (m.hand > 0) ] [ text " Start " ]
         , br [] []
         ]

