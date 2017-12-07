import Html exposing (..)
import Html.Events exposing (..)
import Random
import Html.Attributes exposing (style)
import List.Extra exposing (unique)
import Dict

main = Html.program
    { init            = ([] , Cmd.none)
    , view            = view
    , update          = update
    , subscriptions   = subscriptions
    }

type Msg = Shuffle | Face (List Int)
type alias Deck = List (Int , (Int,Int))

myhashSuit = Dict.fromList [(1,"♠"),(2,"♣"),(3,"♦"),(4,"♥")]
myhashRank = Dict.fromList
             [ (2,"2"), (3,"3"), (4,"4"), (5,"5"), (6,"6"), (7,"7"), (8,"8")
             , (9,"9"), (10,"T"), (11,"J"), (12,"Q"), (13,"K"), (14,"A")
             ]
myhash52 =
  Dict.fromList
    [ (2 ,(4, 2)),(3 ,(4, 3)),(4 ,(4, 4)),(5 ,(4, 5)),(6 ,(4, 6)),(7 ,(4,7)),(8 ,(4,8)),(9 ,(4,9))
    , (10,(4,10)),(11,(4,11)),(12,(4,12)),(13,(4,13)),(14,(4,14))
    , (15,(3, 2)),(16,(3, 3)),(17,(3, 4)),(18,(3, 5)),(19,(3, 6)),(20,(3,7)),(21,(3,8)),(22,(3,9))
    , (23,(3,10)),(24,(3,11)),(25,(3,12)),(26,(3,13)),(27,(3,14))
    , (28,(2, 2)),(29,(2, 3)),(30,(2, 4)),(31,(2, 5)),(32,(2, 6)),(33,(2,7)),(34,(2,8)),(36,(2,9))
    , (36,(2,10)),(37,(2,11)),(38,(2,12)),(39,(2,13)),(40,(2,14))
    , (41,(1, 2)),(42,(1, 3)),(43,(1, 4)),(44,(1, 5)),(45,(1, 6)),(46,(1,7)),(47,(1,8)),(48,(1,9))
    , (49,(1,10)),(50,(1,11)),(51,(1,12)),(52,(1,13)),(53,(1,14))    
    ]
                            
subscriptions : List Int -> Sub Msg
subscriptions m = Sub.none
    
update : Msg -> List Int -> (List Int ,  Cmd Msg)
update b m = case b of
    Shuffle ->
        (m , Random.generate Face hfun0)
    Face n  ->
        (hfun1 n , Cmd.none)

view : List Int -> Html Msg
view x1s =
  let x1  = hfun9 x1s
      x2s = List.drop 2 x1s
      x2  = hfun9 x2s
      x3s = List.drop 2 x2s
      x3  = hfun9 x3s
      x4s = List.drop 2 x3s
      x4  = hfun9 x4s            
      x5s = List.drop 2 x4s
      x5  = hfun9 x5s
      x6s = List.drop 2 x5s
      x6  = hfun9 x6s            
      x7s = List.drop 2 x6s
      x7  = hfun9 x7s           
      x8s = List.drop 2 x7s
      x8  = hfun9 x8s            
      x9s = List.drop 2 x8s
      x9  = hfun9 x9s
      xas = List.drop 2 x9s
      xa  = hfun9 xas            
      xf  = List.take 3 (List.drop 2 xas)
  in div [style [("font-size", "24pt")]]
         [ button [ onClick Shuffle ] [ text "Shuffle" ]
         , div [style [("margin-top","5%"),("margin-left","35%"),("position","absolute")]]
             (hfun2 x1) 
         , div [style [("margin-top","5%"),("margin-left","55%"),("position","absolute")]]
             (hfun2 x2) 
         , div [style [("margin-top","15%"),("margin-left","15%"),("position","absolute")]]
             (hfun2 x3)   
         , div [style [("margin-top","15%"),("margin-left","75%"),("position","absolute")]]
             (hfun2 x4)
         , div [style [("margin-top","25%"),("margin-left","5%"),("position","absolute")]]
             (hfun2 x5) 
         , div [style [("margin-top","25%"),("margin-left","85%"),("position","absolute")]]
             (hfun2 x6) 
         , div [style [("margin-top","35%"),("margin-left","15%"),("position","absolute")]]
             (hfun2 x7)   
         , div [style [("margin-top","35%"),("margin-left","75%"),("position","absolute")]]
             (hfun2 x8)                   
         , div [style [("margin-top","45%"),("margin-left","55%"),("position","absolute")]]
             (hfun2 x9) 
         , div [style [("margin-top","45%"),("margin-left","35%"),("position","absolute")]]
             (hfun2 xa) 
         , div [style [("margin-top","25%"),("margin-left","40%"),("position","absolute")]]
             (hfun2 xf)   
         ]

hfun9 : List Int -> List Int
hfun9 xs = List.reverse (List.sort (List.take 2 xs))
      
hfun0 : Random.Generator (List Int)
hfun0 = Random.list 32000 (Random.int 2 53)

hfun1 : List Int -> List Int
hfun1 xs = List.Extra.unique xs
        
hfun2 : List Int -> List (Html a)
hfun2 xs =
  List.map ( \k1 ->
      let v1 = Dict.get k1 myhash52
      in case v1 of
             Just (k2,k3) ->            
               let v2 = Dict.get k2 myhashSuit
                   v3 = Dict.get k3 myhashRank
               in case v2 of
                      Just suit ->
                          case v3 of
                              Just rank -> 
                                 case k2 of
                                     1 -> span (ty "black") (tt rank suit) 
                                     2 -> span (ty "blue" ) (tt rank suit)
                                     3 -> span (ty "brown") (tt rank suit)
                                     4 -> span (ty "red"  ) (tt rank suit)
                                     _ -> span (ty "white") (tt "" "") 
                              Nothing ->  span (ty "white") (tt "" "") 
                      Nothing ->          span (ty "white") (tt "" "") 
             Nothing ->                   span (ty "white") (tt "" "")  
    ) xs

                 
ty c1    = [style [("color", c1)]]
tt b1 b2 = [text (b1 ++ b2 ++ ". ")]
