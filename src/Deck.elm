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

myhashSuit = Dict.formList [(1,"♠"),(2,"♣"),(3,"♦"),(4,"♥")]
myhashRank = Dict.fromList [(7,"7"),(8,"8"),(9,"9"),(10,"T"),(11,"J"),(12,"Q"),(13,"K"),(14,"A")]
myhash32 =
    Dict.fromList
    [ (9 ,(4,14)),(2 ,(4,7),(3 ,(4,8),(4 ,(4,9),(5 ,(4,10)),(6 ,(4,11)),(7 ,(4,12)),(8 ,(4,13))
    , (17,(3,14)),(10,(3,7),(11,(3,8),(12,(3,9),(13,(3,10)),(14,(3,11)),(15,(3,12)),(16,(3,13))
    , (25,(2,14)),(18,(2,7),(19,(2,8),(20,(2,9),(21,(2,10)),(22,(2,11)),(23,(2,12)),(24,(2,13))
    , (33,(1,14)),(26,(1,7),(27,(1,8),(28,(1,9),(29,(1,10)),(30,(1,11)),(31,(1,12)),(32,(1,13))
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
view xs1 =
  let x1  = hfun9 xs1
      xs2 = List.drop 10 xs1
      x2  = hfun9 xs2
      xs3 = List.drop 10 xs2
      x3  = hfun9 xs3
      x4  = List.reverse (List.sort (List.drop 10 xs3))
  in div [style [("font-size", "24pt")]]
         [ button [ onClick Shuffle ] [ text "Shuffle" ]
         , div [style [("margin-top"," 5%"),("margin-left"," 5%"),("position","absolute")]]
             (hfun2 x1) 
         , div [style [("margin-top"," 5%"),("margin-left","60%"),("position","absolute")]]
             (hfun2 x2) 
         , div [style [("margin-top","14%"),("margin-left","46%"),("position","absolute")]]
             (hfun2 x4)   
         , div [style [("margin-top","25%"),("margin-left","30%"),("position","absolute")]]
             (hfun2 x3)      
         ]

hfun9 : List Int -> List Int
hfun9 xs = List.reverse (List.sort (List.take 10 xs))
      
hfun0 : Random.Generator (List Int)
hfun0 = Random.list 1024 (Random.int 2 33)

hfun1 : List Int -> List Int
hfun1 xs = List.Extra.unique xs
        
hfun2 : List Int -> List (Html a)
hfun2 xs = List.map ( \x ->
  case Dict.get x myhash32 of
     Just (s2,r2) -> let s3 = case Dict.get s1 myhashSuit of
                                Just s3 -> s3
                                Nothing -> ""
                         r3 = case Dict.get r1 myhashRank of
                                Just r3 -> r3
                                Nothing -> ""
     Nothing -> (0,0) 
  in case s2 > 2 of
       True  -> let tyl = [style [("color", "red")]
                    tel = text (s3 ++ r3)
                in span tyl tel                                
       False -> let tyl = [style [("color","blue")]
                    tel = text (s3 ++ r3)
                in span tyl tel 

