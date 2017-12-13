module Model exposing (..)

import Time exposing (..)

-----  Model


type Msg = Start
         | Deal
         | Initial      (List Int)
         | Hand         (List Int)
         | AllIn
         | Bet           String
         | Call
         | Check
         | Fold
         | ChangeBet     String
         | Tick          Time
         | SwitchToRed
         | SwitchToBlue


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

