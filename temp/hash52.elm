old_myhash52 =
    Dict.fromList
        [ (1  , "♥A") , (2  , "♥2") , (3  , "♥3") , (4  , "♥4") , (5  , "♥5") , (6  , "♥6")
        , (7  , "♥7") , (8  , "♥8") , (9  , "♥9") , (10 , "♥T") , (11 , "♥J") , (12 , "♥Q")
        , (13 , "♥K")
        , (14 , "♦A") , (15 , "♦2") , (16 , "♦3") , (17 , "♦4") , (18 , "♦5") , (19 , "♦6")
        , (20 , "♦7") , (21 , "♦8") , (22 , "♦9") , (23 , "♦T") , (24 , "♦J") , (25 , "♦Q")
        , (26 , "♦K")
        , (27 , "♣A") , (28 , "♣2") , (29 , "♣3") , (30 , "♣4") , (31 , "♣5") , (32 , "♣6")
        , (33 , "♣7") , (34 , "♣8") , (35 , "♣9") , (36 , "♣T") , (37 , "♣J") , (38 , "♣Q")
        , (39 , "♣K")
        , (40 , "♠A") , (41 , "♠2") , (42 , "♠3") , (43 , "♠4") , (44 , "♠5") , (45 , "♠6")
        , (46 , "♠7") , (47 , "♠8") , (48 , "♠9") , (49 , "♠T") , (50 , "♠J") , (51 , "♠Q")
        , (52 , "♠K")             
        ]


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
    , (28,(2, 2)),(29,(2, 3)),(30,(2, 4)),(31,(2, 5)),(32,(2, 6)),(33,(2,7)),(34,(2,8)),(35,(2,9))
    , (36,(2,10)),(37,(2,11)),(38,(2,12)),(39,(2,13)),(40,(2,14))
    , (41,(1, 2)),(42,(1, 3)),(43,(1, 4)),(44,(1, 5)),(45,(1, 6)),(46,(1,7)),(47,(1,8)),(48,(1,9))
    , (49,(1,10)),(50,(1,11)),(51,(1,12)),(52,(1,13)),(53,(1,14))    
    ]        