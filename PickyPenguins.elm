import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time
import Window
import Keyboard
import Signal exposing ((<~),(~))
import Json.Decode as Json exposing ((:=),Decoder)
    
{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type alias UserInput =
    { dx : Int
    , dy : Int}

userInput : Signal UserInput
userInput = UserInput <~ (.x <~ Keyboard.arrows) ~ (.y <~ Keyboard.arrows)

type Input = TimeDelta Float | UserAction UserInput

{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }

------------------------------------------------------------------------------}

type GameColor = Red | Green | Blue

type BoxAction = BoxMove | BoxDisappear
type GoalAction = GoalDisappear

type alias Positioned a = { a | x : Int, y : Int}
type alias Colored a = { a | color : GameColor}

type alias Box = Positioned (Colored
    { boxActions : List BoxAction})

makeBox : Int -> Int -> GameColor -> Box
makeBox x y color =
    {x = x, y = y, color = color, boxActions = []}

type alias Goal = Positioned (Colored
    { goalActions : List GoalAction})

makeGoal : Int -> Int -> GameColor -> Goal
makeGoal x y color =
    {x = x, y = y, color = color, goalActions = []}

type alias Wall = Positioned
    {}

makeWall : Int -> Int -> Wall
makeWall x y = {x = x, y = y}

type alias Direction =
    { dx : Int
    , dy : Int}

type alias Dimensions =
    { dimx : Int
    , dimy : Int}

makeDimensions : Int -> Int -> Dimensions
makeDimensions dimx dimy = {dimx = dimx, dimy = dimy}

type alias Level =
    { boxes : List Box
    , goals : List Goal
    , walls : List Wall
    , dimensions : Dimensions}

makeLevel : List Box -> List Goal -> List Wall -> Dimensions -> Level
makeLevel boxes goals walls dimensions =
    {boxes = boxes, goals = goals, walls = walls, dimensions = dimensions}

type alias GameState =
    { boxes : List Box
    , goals : List Goal
    , walls : List Wall
    , direction : Direction
    , dimensions : Dimensions
    , frame : Int}

still : Direction
still = { dx = 0, dy = 0}

levelString : String
levelString = "{\"boxes\": [{\"x\": 3, \"y\": 5, \"color\": \"red\"}], \"goals\": [{\"x\" : 6, \"y\": 1, \"color\": \"green\"}], \"walls\": [{\"x\" : 2, \"y\": 3}], \"dimensions\": {\"width\": 10, \"height\": 20}}"

stringToColor : String -> GameColor
stringToColor s =
    if | s == "red" -> Red
       | s == "green" -> Green
       | s == "blue" -> Blue

colorDecoder : Decoder GameColor
colorDecoder = Json.map stringToColor Json.string

boxDecoder : Decoder Box
boxDecoder = Json.object3 makeBox
             ("x" := Json.int)
             ("y" := Json.int)
             ("color" := colorDecoder)

goalDecoder : Decoder Goal
goalDecoder = Json.object3 makeGoal
              ("x" := Json.int)
              ("y" := Json.int)
              ("color" := colorDecoder)

wallDecoder : Decoder Wall
wallDecoder = Json.object2 makeWall
              ("x" := Json.int)
              ("y" := Json.int)

dimensionsDecoder : Decoder Dimensions
dimensionsDecoder = Json.object2 makeDimensions
                    ("width" := Json.int)
                    ("height" := Json.int)

levelDecoder : Decoder Level
levelDecoder = Json.object4 makeLevel
               ("boxes" := Json.list boxDecoder)
               ("goals" := Json.list goalDecoder)
               ("walls" := Json.list wallDecoder)
               ("dimensions" := dimensionsDecoder)

level : Result String Level
level = Json.decodeString levelDecoder levelString
        
defaultGame : Result String GameState
defaultGame = case level of
                Ok l -> Ok
                        { boxes = l.boxes
                        , goals = l.goals
                        , walls = l.walls
                        , dimensions = l.dimensions
                        , direction = still
                        , frame = numFrames - 1}
                Err msg -> Err msg


{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

numFrames = 8

occupiesSameSpot : Positioned a -> Positioned b -> Bool
occupiesSameSpot p1 p2 = p1.x == p2.x && p1.y == p2.y

inBounds : Dimensions -> Box -> Bool
inBounds {dimx,dimy} {x,y} =
    x >= 0 && x < dimx && y >= 0 && y < dimy

clearOfPositioned : List (Positioned a) -> Box -> Bool
clearOfPositioned xs box = xs
                 |> List.filter (occupiesSameSpot box)
                 |> List.isEmpty

-- Generic such that a list of goals can be passed for the box or a list of boxes can be found for the goal.
reachedGoal : List (Colored (Positioned a)) -> (Colored (Positioned b)) -> Bool
reachedGoal goals box = goals
                      |> List.filter (occupiesSameSpot box)
                      |> List.map .color
                      |> List.member box.color

determineBoxAction : GameState -> Box -> List BoxAction
determineBoxAction gameState box =
    let movedBox = moveBox gameState.direction box
    in if | reachedGoal gameState.goals box -> [BoxDisappear]
          | not <| inBounds gameState.dimensions movedBox -> []
          | not <| clearOfPositioned gameState.walls movedBox -> []
          | not <| clearOfPositioned gameState.boxes movedBox -> []
          | otherwise -> [BoxMove]

tagBoxAction : GameState -> Box -> Box
tagBoxAction gameState box = { box | boxActions <- determineBoxAction gameState box}

determineGoalAction : GameState -> Goal -> List GoalAction
determineGoalAction gameState goal =
    if reachedGoal gameState.boxes goal then [GoalDisappear] else []

tagGoalAction : GameState -> Goal -> Goal
tagGoalAction gameState goal = { goal | goalActions <- determineGoalAction gameState goal}
       
moveBox : Direction -> Box -> Box
moveBox {dx,dy} ({x,y} as box) =
    { box | x <- x + dx,
            y <- y + dy}

comp : (a -> Bool) -> a -> Bool
comp f x = not (f x)

determineActions : GameState -> GameState
determineActions gameState =
    { gameState | boxes <- List.map (tagBoxAction gameState) gameState.boxes,
                  goals <- List.map (tagGoalAction gameState) gameState.goals}

applyBoxActions : GameState -> GameState
applyBoxActions gameState =
    let remaining = List.filter (comp (List.member BoxDisappear) << .boxActions) gameState.boxes
        mobile = List.filter (List.member BoxMove << .boxActions) remaining
        immobile = List.filter ((List.member BoxMove << .boxActions) |> comp) remaining
    in {gameState | boxes <- immobile ++ List.map (moveBox gameState.direction) mobile,
                    direction <- if List.isEmpty mobile then still else gameState.direction}

applyGoalActions : GameState -> GameState
applyGoalActions gameState =
    {gameState | goals <- List.filter (comp (List.member GoalDisappear) << .goalActions) gameState.goals}

applyActions : GameState -> GameState
applyActions gameState = gameState |> applyBoxActions |> applyGoalActions

tryChangeDirection : UserInput -> GameState -> GameState
tryChangeDirection input gameState =
    if (gameState.direction == still)
      then { gameState | direction <- input }
      else gameState

stepFrame : GameState -> GameState
stepFrame gameState =
    if gameState.direction == still
    then gameState
    else { gameState | frame <- (gameState.frame + 1) % numFrames}

stepGame : Input -> GameState -> GameState
stepGame input gameState =
    case input of
      TimeDelta _ -> if gameState.frame == numFrames - 1 then applyActions gameState |> determineActions else gameState |> stepFrame
      UserAction userInput -> gameState |> tryChangeDirection userInput |> determineActions

stepResultGame : Input -> Result String GameState -> Result String GameState
stepResultGame input = Result.map (stepGame input)

{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

gameDimensions : (Int,Int)
gameDimensions = (500,400)
(gameWidth,gameHeight) = gameDimensions
(halfWidth,halfHeight) = (gameWidth // 2, gameHeight // 2)

tupFloat : (Int,Int) -> (Float,Float)
tupFloat (a,b) = (toFloat a, toFloat b)
                         
dispColor : GameColor -> Color
dispColor color = case color of
                    Red -> rgb 255 0 0
                    Green -> rgb 0 255 0
                    Blue -> rgb 0 0 255

backgroundColor : Color
backgroundColor = rgb 50 80 100

wallColor : Color
wallColor = rgb 255 255 255

irect = tupFloat >> uncurry rect

cellDimensions : Dimensions -> (Int,Int)
cellDimensions {dimx,dimy} = (gameWidth // dimx, gameHeight // dimy)

cellPosition (cellWidth,cellHeight) {x,y} =
    (x * cellWidth - halfWidth + cellWidth // 2, y * cellHeight - halfHeight + cellHeight // 2)

displayBox cellDim box = cellDim |> irect |> filled (dispColor box.color) |> move (cellPosition cellDim box |> tupFloat)

tweenVec {dx,dy} (cellWidth,cellHeight) frame =
    (dx * cellWidth * frame // numFrames, dy * cellHeight * frame // numFrames) |> tupFloat

tweenBox cellDim box gameState element =
    if List.member BoxMove box.boxActions
    then move (tweenVec gameState.direction cellDim gameState.frame) element
    else element

displayWall cellDim wall = cellDim |> irect |> filled wallColor |> move (cellPosition cellDim wall |> tupFloat)

displayGoal cellDim goal = cellDim |> tupFloat |> uncurry oval |> filled (dispColor goal.color) |> move (cellPosition cellDim goal |> tupFloat)

displayGame : (Int,Int) -> GameState -> Element
displayGame (w,h) gameState =
    let cellDim = cellDimensions gameState.dimensions
    in container w h middle <|
       collage gameWidth gameHeight
                   ([ gameDimensions |> irect |> filled backgroundColor]
                    ++ List.map (\ b -> displayBox cellDim b |> tweenBox cellDim b gameState) gameState.boxes
                    ++ List.map (displayWall cellDim) gameState.walls
                    ++ List.map (displayGoal cellDim) gameState.goals)

display : (Int,Int) -> Result String GameState -> Element
-- display (w,h) gameState =
--     show gameState
display dim gameResultState =
    case gameResultState of
      Ok gameState -> displayGame dim gameState
      Err msg -> show msg

{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta : Signal Float
delta = Time.fps 30

input : Signal Input
input = Signal.merge (TimeDelta <~ delta) (UserAction <~ userInput)

gameState : Signal (Result String GameState)
gameState =
    Signal.foldp stepResultGame defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ gameState