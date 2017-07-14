type Msg = Tick Float GetKeyState
         | StartGame
         | SubmitAnswer Answer Answer
         | Reset
         | Instructions
         | Back

type GameState = MainMenu
               | InGame
               | EndOfGame
               | Failure
               | Instruction
          
type Answer = A | B | C | D

main =
    gameApp Tick {   model = init
                 ,   view = view
                 ,   update = update
                 }

--- MODEL ---

init = { state = MainMenu
       , levels = [ level1,
                    level2,
                    level3,
                    level4,
                    level5,
                    level6
                    
                  ]
       , chances = 2
       , time = 0  -- This is specifically ANIMATION time.

         -- Below are a set of variables that aren't used in the template but
         -- maybe you can figure out how to use them? You can add more too!
       , score = 0
       , timelimit = 20
       , highscore = 0
       , current = 0
       }

--- VIEW ---

view model = case model.state of
                MainMenu -> collage 1000 500 (menuView model)
                InGame   -> collage 1000 500 (levelView (List.head model.levels) model.time model.chances model.timelimit)
                EndOfGame   -> collage 1000 500 (endView model)
                Failure  -> collage 1000 500 (failView model)
                Instruction -> collage 1000 500 (instruction model)
                
menuView model = [ group [ rect 320 320 
                            |> filled darkBrown
                            , rect 320 21 |> filled green |> move (0,150)
                            
                            , rect 20 21 |> filled green |> move (150,130)
                            , rect 21 41 |> filled green |> move (130,120)
                            , rect 21 41 |> filled green |> move (110,120)
                            , rect 21 21 |> filled green |> move (90,130)
                            , rect 21 41 |> filled green |> move (70,120)
                            , rect 21 61 |> filled green |> move (50,110)
                            , rect 21 41 |> filled green |> move (30,120)
                            , rect 21 61 |> filled green |> move (10,110)
                            
                            , rect 21 21 |> filled green |> move (-10,130)
                            , rect 21 41 |> filled green |> move (-30,120)
                           
                            , rect 21 61 |> filled green |> move (-70,110)
                            , rect 21 41 |> filled green |> move (-90,120)
                            , rect 21 41 |> filled green |> move (-110,120)
                            , rect 21 21 |> filled green |> move (-130,130)
                            , rect 20 41 |> filled green |> move (-150,120)
                           
                         
                         , text "START"
                            |> size 50
                            |> centered
                            |> filled white
                            |> move (0,-15)
                         ]  |> notifyMouseDown StartGame |> move (0,50)
                         
                         , group [ rect 220 80 |> filled lightCharcoal
                            ,text "Instructions"
                            |> size 40
                            |> centered
                            |> filled white 
                            |> move (0,-10)
                            |> notifyMouseDown Instructions 
                            
                       
                        
                        
                        
                        ] |> move(0,-50)
                 ]        

instruction model =   [      text "simply choose the right answer with the proper spelling."
                            
                            |> size 40
                            |> centered
                            |> filled black                             
                            |> move (0,-5)
                            ,text "Getting a wrong answer will make you lose a life."
                            |> size 40
                            |> centered
                            |> filled black                             
                            |> move (0,-50)
                            , rect 120 60 |> filled lightCharcoal |> move (0,-120) |> notifyMouseDown Back 
                            , text "Back" |> size 30 |> centered |> filled black |> move (0,-130) |> notifyMouseDown Back
                             ]



endView model = [ group [ rect 250 250
                            |> filled black
                            |> addOutline (solid 6) green
                         , text "RESET"
                            |> size 50
                            |> centered
                            |> filled white
                            |> move (0,-15)
                         ] |> notifyMouseDown Reset
                            ,rect 80 10 |> filled green |> move (0,-90)
                            ,rect 10 15 |> filled green |> move (-35,-80)
                            ,rect 10 15 |> filled green |> move (35,-80)
                            ,rect 10 10 |> filled green |> move (-15,-35)
                            ,rect 10 10 |> filled green |> move (15,-35)]

failView model = [ group [ rect 250 250
                            |> filled black
                            |> addOutline (solid 6) red
                         , text "RESET"
                            |> size 50
                            |> centered
                            |> filled white
                            |> move (0,-15)
                         ] |> notifyMouseDown Reset
                            ,rect 80 60 |> filled white |> move (0,-70)
                            ,rect 15 15 |> filled red |> move (-20,-70)
                            ,rect 15 15 |> filled red |> move (20,-70)
                            ,rect 10 10 |> filled white |> move (0,-104)
                            ,rect 10 10 |> filled white |> move (-20,-104)
                            ,rect 10 10 |> filled white |> move (20,-104)
                            ]





levelView level t chances timelimit = case level of
                                 Nothing -> []
                                 Just lev ->  [ group (lev.image t)
                                            , option A lev.optionA
                                                |> move (-150,-120)
                                                |> notifyMouseDown (SubmitAnswer A lev.answer)
                                            , option B lev.optionB
                                                |> move (-150,-160)
                                                |> notifyMouseDown (SubmitAnswer B lev.answer)
                                            , option C lev.optionC
                                                |> move (150,-120)
                                                |> notifyMouseDown (SubmitAnswer C lev.answer)
                                            , option D lev.optionD
                                                |> move (150,-160)
                                                |> notifyMouseDown (SubmitAnswer D lev.answer)
                                            , text "Extra chances"
                                                |> filled red
                                                |> move (200,100)
                                            , group (displayChances chances)
                                                |> move (200,150)
                                            , rect 50 40|> filled lightCharcoal |> move (-185,210)
                                            , text (toString (ceiling timelimit))
                                                |> size 30
                                                |> filled black
                                                |> move (-200,200)
                                               
                                           
                                           ]

displayChances chances = case chances of
                            0 -> []
                            _ -> [heart red
                                    |> scale (0.5)
                                    |> move (0 +  chances * 100,0) 
                                    ] ++ (displayChances (chances - 1))
                               


option ans tex = group [ rectangle 200 30
                            |> filled lightCharcoal
                       , text ((toString ans) ++ ": " ++ tex)
                            |> size 20
                            |> filled white
                            |> move (-90,-7) ]


level1 = { image = level1_image
         , optionA = "Creeper"
         , optionB = "Zombie"
         , optionC = "Zonbie"
         , optionD = "Zomble"
         , answer = B
         }

level1_image t = [ zombie green ]

level2 = { image = level2_image
         , optionA = "Skeletron"
         , optionB = "Skaleton"
         , optionC = "Skeleton"
         , optionD = "Skeloton"
         , answer = C
         }

level2_image t = [ skeleton white  ]

level3 = { image = level3_image
         , optionA = "Slenderman"
         , optionB = "Endermen"
         , optionC = "Enderman"
         , optionD = "Endarman"
         , answer = C
         }

level3_image t = [ enderman purple ]

level4 = { image = level4_image
         , optionA = "Creeper"
         , optionB = "Creper"
         , optionC = "Creaper"
         , optionD = "creeper"
         , answer = A
         }

level4_image t = [if 0 < (round (t*10) % 50) || (round (t*10) % 50) > 25
                  then creeper white 
                  else creeperflash green ]

level5 = { image = level5_image
         , optionA = "Maqma Cube"
         , optionB = "Magna Cube"
         , optionC = "Magma cube"
         , optionD = "Magma Cube"
         , answer = D
         }

level5_image t = [if 0 < (round (t/3) % 50) || (round (t/3) % 50) > 25
                  then magmacube1 red 
                  else magmacube red |> move (0,50)]



level6 = { image = level6_image
         , optionA = "Vindicator"
         , optionB = "Villager"
         , optionC = "Illusioner"
         , optionD = "Evoker"
         , answer = A
         }
         
level6_image t = [ illager blue |> scale (abs (sin (t/20)) + 0.5)]



creeper c = group [rect 80 80|> filled green
            ,rect 20 20 |> filled black |> move (0,-10)
            ,rect 20 20 |> filled black |> move (20,10)
            ,rect 20 20 |> filled black |> move (-20,10)
            ,rect 10 20 |> filled black |> move (14.9,-20)
            ,rect 10 20 |> filled black |> move (-14.9,-20)] 

creeperflash c = rect 80 80 |> filled (rgb 254 254 254) 

zombie c = group [rect 80 80 |> filled darkGreen
           ,rect 20 10 |> filled black |> move (-20,-5)
           ,rect 20 10 |> filled black |> move (20,-5)]

skeleton c = group [rect 80 80 |> filled white |> addOutline (solid 1) black
            ,rect 20 10 |> filled black |> move (-20,-5)
            ,rect 20 10 |> filled black |> move (20,-5)
            ,rect 20 10 |> filled gray |> move (0,-15)
            ,rect 60 10 |> filled black |> move (0,-25)
            ] 

enderman c = group [rect 80 80 |> filled black |> addOutline (solid 1) black
                 ,rect 30 10 |> filled white |> move (-25,-5)               
                 ,rect 30 10 |> filled white |> move (25,-5)
                 ,rect 10 10 |> filled (rgb 255 63 255) |> move (-25,-5)
                 ,rect 10 10 |> filled (rgb 255 63 255) |> move (25,-5)]

magmacube c = group [rect 80 10 |> filled (rgb 102 40 40) |> move (0,70)
                    ,rect 80 10 |> filled (rgb 102 40 40) |> move (0,50)
                    ,rect 80 10 |> filled (rgb 102 40 40) |> move (0,30)
                    
                    ,rect 80 10 |> filled (rgb 102 40 40) |> move (0,10)
                    ,rect 80 10 |> filled (rgb 102 40 40) |> move (0,-10)
                    ,rect 80 10 |> filled (rgb 102 40 40) |> move (0,-30)
                    ,rect 80 10 |> filled (rgb 102 40 40) |> move (0,-50)
                    ,rect 80 10 |> filled (rgb 102 40 40) |> move (0,-70)
                    ,rect 20 10 |> filled (rgb 255 102 0) |> move (20,30)
                    ,rect 20 10 |> filled (rgb 255 102 0) |> move (-20,30)
                    ,rect 20 10 |> filled (rgb 255 102 0) |> move (20,10)
                    ,rect 20 10 |> filled (rgb 255 102 0) |> move (-20,10)
                    
                    
                    ,rect 10 10 |> filled yellow |> move (15,10)
                    ,rect 10 10 |> filled yellow |> move (-15,10)]
                    
magmacube1 c = group [rect 80 80 |> filled (rgb 102 40 40)
                    ,rect 20 20 |> filled (rgb 255 102 0) |> move (-20,10)
                    ,rect 20 20 |> filled (rgb 255 102 0) |> move (20,10)
                    ,rect 10 10 |> filled yellow |> move (-15,5)
                    ,rect 10 10 |> filled yellow |> move (15,5) ]

illager c = group [rect 80 100 |> filled lightCharcoal
                  ,rect 40 10 |> filled darkCharcoal |> move (0,-35)
                  ,rect 20 40 |> filled charcoal |> move (0,-40)
                  ,rect 20 10 |> filled white |> move (-20,-15)
                  ,rect 20 10 |> filled white |> move (20,-15)
                  ,rect 11 11 |> filled (rgb 0 119 145) |> move (-15,-15)
                  ,rect 11 11 |> filled (rgb 0 119 145) |> move (15,-15)
                  ,rect 20 20 |> filled (rgb 53 53 53) |> move (-30,0)
                  ,rect 20 20 |> filled (rgb 53 53 53) |> move (30,0)
                  ,rect 20 11 |> filled (rgb 53 53 53) |> move (-20,-5)
                  ,rect 20 11 |> filled (rgb 53 53 53) |> move (20,-5)
                  ,rect 10 30 |> filled charcoal |> move (-35,-35)
                  ,rect 10 30 |> filled charcoal |> move (35,-35)]


heart c = group [
          rect 70 30 |> filled red
         ,rect 20 50 |> filled red |> move (15,0)
         ,rect 20 50 |> filled red |> move (-15,0) 
         ,rect 30 30 |> filled red |> move (0,-20)
         ,rect 12 25 |> filled red |> move (0,-30)
         ,rect 10 10 |> filled white |> move (-20,10)]


-- scale (abs (sin (t/20)) + 0.5) 

--- UPDATE ---

update msg model = case msg of
                        Tick t _ -> if model.timelimit <= 0 then timeRanOut model else 
                                    { model | state = if model.state == InGame && model.levels == []
                                                            then EndOfGame
                                                            else model.state
                                    ,         time = model.time + 1
                                    ,         timelimit = if model.state == InGame then model.timelimit - 0.1 else model.timelimit}
                        StartGame -> { model | state = InGame}
                        SubmitAnswer ans1 ans2 -> if ans1 == ans2
                                                    then nextLevel model
                                                    else wrongAnswer model
                        Reset -> init
                        Instructions -> { model | state = Instruction}
                        Back -> { model | state = MainMenu}
                        
                         
nextLevel model = {model | levels = Maybe.withDefault [] (List.tail model.levels) , time = 0}

wrongAnswer model = case model.chances of
                        0 -> {model | state = Failure}
                        _ -> {model | chances = model.chances - 1}
                        
                        
timeRanOut model = case model.chances of
                        0 -> {model | state = Failure}
                        _ -> {model | chances = model.chances - 1, timelimit = 10}

