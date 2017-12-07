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
type alias Deck = List (Int , String)
    
myhash32 =
    Dict.fromList
    [ (9 ,"♥A"),(2 ,"♥7"),(3 ,"♥8"),(4 ,"♥9"),(5 ,"♥T"),(6 ,"♥J"),(7 ,"♥Q"),(8 ,"♥K")
    , (17,"♦A"),(10,"♦7"),(11,"♦8"),(12,"♦9"),(13,"♦T"),(14,"♦J"),(15,"♦Q"),(16,"♦K")
    , (25,"♣A"),(18,"♣7"),(19,"♣8"),(20,"♣9"),(21,"♣T"),(22,"♣J"),(23,"♣Q"),(24,"♣K")
    , (33,"♠A"),(26,"♠7"),(27,"♠8"),(28,"♠9"),(29,"♠T"),(30,"♠J"),(31,"♠Q"),(32,"♠K")
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
hfun2 xs = List.map (\x -> let s2 = case Dict.get x myhash32 of
                                      Just s1 -> s1
                                      Nothing -> " " 
                           in text ("  " ++ s2)) xs

