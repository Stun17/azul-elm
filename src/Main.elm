module Main exposing (main)

{-|
@docs main
-}

import Dict
import Random
import List.Extra exposing (unique)
import Html exposing (program)
import Time exposing (Time, second)

import Mydeck exposing (..)
import Update exposing (update)
import View exposing (view)
import Model exposing (..)

--------------------------

{-|
@docs 
-}
main : Program Never Struc Msg
main = Html.program
    { init            = init
    , subscriptions   = subscriptions
    , update          = update
    , view            = view
    }

subscriptions : Struc -> Sub Msg
subscriptions m = Time.every 10000 Tick   -- Time.every second

init : (Struc, Cmd a)
init = ({ ppocket = [(0,0), (0,0)], pstack = 200, pstatus = Id
        , kpocket = [(0,0), (0,0)], kstack = 200, kstatus = Id
        , pot = 0, board = [(0,0), (0,0), (0,0), (0,0), (0,0)]
        , hand = 0, order = False, bet = 1, gstage = St
        , pdeal = False, timer = 0, block = False, deck = True
        } , Cmd.none)

