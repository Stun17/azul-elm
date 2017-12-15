module Update exposing (update)

import Dict
import Random
import List.Extra exposing (unique)
import Html exposing (..)
import Time exposing (Time, second)
import MyHashes exposing (..)
import Model exposing (..)

update : Msg -> Struc -> (Struc , Cmd Msg)
update b s = case b of
  SwitchToRed ->
      ( { s | deck = False }, Cmd.none )
  SwitchToBlue ->
      ( { s | deck = True  }, Cmd.none )
  Start ->                                                  -- готовим ряд для формирования колоды
        (s, Random.generate Initial (Random.list 255 (Random.int 2 53)))
  Initial ys ->                     
     let zs = mfun0 ys
     in case (List.take 1 zs) of                                        -- кто первым сдает ?
          [(s1, r1)] -> case (List.take 1 (List.drop 1 zs)) of
             [(s2, r2)] -> ( { s |
                              pdeal    = r2 > r1
                            , ppocket  = [(s2, r2)] , kpocket = [(s1, r1)]
                            , pstatus  = Id         , kstatus = Id
                            , gstage   = Dc         , order = False
                            }, Cmd.none )
             _ -> (s, Cmd.none )
          _ -> (s, Cmd.none )
  Deal ->
    (s , Random.generate Hand (Random.list 255 (Random.int 2 53)))  -- ряд для формирования колоды
  Hand ys ->                                                         -- начало каждой раздачи
     let zs = mfun0 ys                                               -- формируем колоду
     in ( { s |
            ppocket = List.take 2 zs                                 -- формируем руку игрока
          , kpocket = List.take 2 (List.drop 2 zs)                   -- формируем руку бота
          , board   = List.take 5 (List.drop 4 zs)                   -- формируем боард

          , pstatus = if s.pdeal then Th else Id
          , kstatus = if s.pdeal then Id else Th

          , pdeal   = if s.hand > 0 then not s.pdeal else s.pdeal

          , pot     = 3 , bet = 1                                      -- анте
          , pstack  = if s.pdeal then s.pstack - 1 else s.pstack - 2   -- анте
          , kstack  = if s.pdeal then s.pstack - 2 else s.kstack - 1   -- анте
          , hand    = s.hand + 1                                       -- счетчик раздач

          , order   = s.pdeal , round = 1
          , gstage  = Pr
          } , Cmd.none )
  Fold ->
     ( { s |
         kstack  = s.pot + s.kstack
       , pot     = 0
       , pstatus = Fo
       , kstatus = Wi
       , order   = False , round = 1
       -- , gstage  = Dd
       -- , pdeal   = not s.pdeal
       }, Cmd.none )
  Check ->
    ( { s |
        pstatus = Ch
      , kstatus = Th
      , order   = False , round = 1
      }, Cmd.none )
  Call ->
    ( { s |
        pot     = s.pot + s.bet
      , pstack  = s.pstack - s.bet
      , bet     = if (s.pdeal && s.gstage == Pr) then 2 else s.bet
      , pstatus = Ca
      , kstatus = Th
      , order   = False , round = s.round + 1
      } , Cmd.none )
  Bet x ->
     let z = Result.withDefault s.bet (String.toInt x)
     in ( { s |
            pot     = s.pot + z
          , pstack  = s.pstack - z
          , bet     = z
          , pstatus = Be
          , kstatus = Th
          , order   = False , round = s.round + 1
          } , Cmd.none )
  ChangeBet x ->
     let y = Result.withDefault s.bet (String.toInt x)
         z =  if y > s.pstack then s.pstack else y 
     in  ( { s | bet = z, block = True }, Cmd.none )
  AllIn ->
    if s.pstack > s.kstack
    then ( { s |
             bet     = s.kstack
           , pot     = s.pot + s.kstack
           , pstack  = s.pstack - s.kstack
           , pstatus = Al
           , kstatus = Th
           , order   = False , round = s.round + 1
           } , Cmd.none )
     else ({ s |
             bet     = s.pstack
           , pot     = s.pot + s.pstack
           , pstack  = 0
           , pstatus = Al
           , kstatus = Th
           , order   = False , round = s.round + 1
           } , Cmd.none )
  Tick x ->
    ( { s |
         kstatus = if s.kstatus == Wi || s.kstatus == Fo then Id else s.kstatus
      ,  pstatus = if s.pstatus == Wi || s.pstatus == Fo then Id else s.pstatus
      ,  gstage  = if s.pstatus == Wi || s.kstatus == Wi then Sd else s.gstage
      ,  timer = s.timer + 1
      }, Cmd.none )


-- функция формирования колоды из списка целых чисел с использованием хеша,
--        где ключ - целое число , а значение - кортеж целых чисел масть/ранг
mfun0 : List Int -> List (Int , Int)
mfun0 ys =
  List.map (\k -> case (Dict.get k myhash52) of
                    Just (s, r) -> (s, r)
                    Nothing     -> (0, 0) )
           (List.Extra.unique ys)

