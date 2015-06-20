import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time
import Window
import Keyboard
import Signal exposing ((<~),(~))
import Json.Decode as Json exposing ((:=),Decoder)
import Http
import Task exposing (Task, andThen)

{-- Input Model --------------------------------------------------------------
------------------------------------------------------------------------------}

type alias UserInput =
    { dx : Int
    , dy : Int}

userInput : Signal UserInput
userInput = UserInput <~ (.x <~ Keyboard.arrows) ~ (.y <~ Keyboard.arrows)

type Input = TimeDelta Float | UserAction UserInput | LevelLoaded LevelId Level

{-- Game Model ---------------------------------------------------------------
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

type alias LevelState =
    { boxes : List Box
    , goals : List Goal
    , walls : List Wall
    , direction : Direction
    , dimensions : Dimensions
    , frame : Int}

still : Direction
still = { dx = 0, dy = 0}

type alias LevelId = Int

type GameState = LoadLevel LevelId | PlayingLevel LevelState | GameError String

{-- JSON definition ----------------------------------------------------------
------------------------------------------------------------------------------}

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

{-- Starting conditions ------------------------------------------------------
------------------------------------------------------------------------------}
        
defaultGame : GameState
defaultGame = LoadLevel 0

{-- Update the game ----------------------------------------------------------
------------------------------------------------------------------------------}

loadLevel : Level -> LevelState
loadLevel l = { boxes = l.boxes
              , goals = l.goals
              , walls = l.walls
              , dimensions = l.dimensions
              , direction = still
              , frame = numFrames - 1}

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

determineBoxAction : LevelState -> Box -> List BoxAction
determineBoxAction levelState box =
    let movedBox = moveBox levelState.direction box
    in if | reachedGoal levelState.goals box -> [BoxDisappear]
          | not <| inBounds levelState.dimensions movedBox -> []
          | not <| clearOfPositioned levelState.walls movedBox -> []
          | not <| clearOfPositioned levelState.boxes movedBox -> []
          | otherwise -> [BoxMove]

tagBoxAction : LevelState -> Box -> Box
tagBoxAction levelState box = { box | boxActions <- determineBoxAction levelState box}

determineGoalAction : LevelState -> Goal -> List GoalAction
determineGoalAction levelState goal =
    if reachedGoal levelState.boxes goal then [GoalDisappear] else []

tagGoalAction : LevelState -> Goal -> Goal
tagGoalAction levelState goal = { goal | goalActions <- determineGoalAction levelState goal}
       
moveBox : Direction -> Box -> Box
moveBox {dx,dy} ({x,y} as box) =
    { box | x <- x + dx,
            y <- y + dy}

comp : (a -> Bool) -> a -> Bool
comp f x = not (f x)

determineActions : LevelState -> LevelState
determineActions levelState =
    { levelState | boxes <- List.map (tagBoxAction levelState) levelState.boxes,
                  goals <- List.map (tagGoalAction levelState) levelState.goals}

applyBoxActions : LevelState -> LevelState
applyBoxActions levelState =
    let remaining = List.filter (comp (List.member BoxDisappear) << .boxActions) levelState.boxes
        mobile = List.filter (List.member BoxMove << .boxActions) remaining
        immobile = List.filter ((List.member BoxMove << .boxActions) |> comp) remaining
    in {levelState | boxes <- immobile ++ List.map (moveBox levelState.direction) mobile,
                    direction <- if List.isEmpty mobile then still else levelState.direction}

applyGoalActions : LevelState -> LevelState
applyGoalActions levelState =
    {levelState | goals <- List.filter (comp (List.member GoalDisappear) << .goalActions) levelState.goals}

applyActions : LevelState -> LevelState
applyActions levelState = levelState |> applyBoxActions |> applyGoalActions

tryChangeDirection : UserInput -> LevelState -> LevelState
tryChangeDirection input levelState =
    if (levelState.direction == still)
      then { levelState | direction <- input }
      else levelState

stepFrame : LevelState -> LevelState
stepFrame levelState =
    if levelState.direction == still
    then levelState
    else { levelState | frame <- (levelState.frame + 1) % numFrames}

stepLevel : Input -> LevelState -> GameState
stepLevel input levelState =
    case input of
      TimeDelta _ -> PlayingLevel (if levelState.frame == numFrames - 1 then applyActions levelState |> determineActions else levelState |> stepFrame)
      UserAction userInput -> PlayingLevel (levelState |> tryChangeDirection userInput |> determineActions)
      LevelLoaded _ _ -> PlayingLevel levelState

checkLevelLoaded : Input -> GameState -> GameState
checkLevelLoaded input gameState =
    case input of
      TimeDelta _ -> gameState
      UserAction _ -> gameState
      LevelLoaded _ lvl -> PlayingLevel <| loadLevel lvl

stepGameState : Input -> GameState -> GameState
stepGameState input gameState = case gameState of
                                  LoadLevel x -> checkLevelLoaded input gameState
                                  PlayingLevel lvl -> stepLevel input lvl
                                  GameError msg -> GameError msg

{-- Display the game ---------------------------------------------------------
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

tweenBox cellDim box levelState element =
    if List.member BoxMove box.boxActions
    then move (tweenVec levelState.direction cellDim levelState.frame) element
    else element

displayWall cellDim wall = cellDim |> irect |> filled wallColor |> move (cellPosition cellDim wall |> tupFloat)

displayGoal cellDim goal = cellDim |> tupFloat |> uncurry oval |> filled (dispColor goal.color) |> move (cellPosition cellDim goal |> tupFloat)

displayGame : (Int,Int) -> LevelState -> Element
displayGame (w,h) levelState =
    let cellDim = cellDimensions levelState.dimensions
    in container w h middle <|
       collage gameWidth gameHeight
                   ([ gameDimensions |> irect |> filled backgroundColor]
                    ++ List.map (\ b -> displayBox cellDim b |> tweenBox cellDim b levelState) levelState.boxes
                    ++ List.map (displayWall cellDim) levelState.walls
                    ++ List.map (displayGoal cellDim) levelState.goals)

display : (Int,Int) -> GameState -> Element
display dim gameState =
    case gameState of
      LoadLevel id -> show <| "Should load level " ++ (toString id)
      PlayingLevel levelState -> displayGame dim levelState
      GameError msg -> show msg

-- display dim gameState =
--     show gameState

{-- Rest urls ----------------------------------------------------------------
------------------------------------------------------------------------------}

levelUrl : LevelId -> String
levelUrl id = Http.url "/levels/" [("id", toString id)]

{-- Wire up level querying ---------------------------------------------------
------------------------------------------------------------------------------}

currentLevel : Signal.Mailbox Level
currentLevel = Signal.mailbox {boxes = [], walls = [], goals = [], dimensions = {dimx = 0, dimy = 0}}

port queryLevel : Signal (Task Http.Error ())
port queryLevel =
    levelQuery.signal
        |> Signal.map getLevel
        |> Signal.map (\task -> task `andThen` Signal.send currentLevel.address)

levelQuery : Signal.Mailbox LevelId
levelQuery = Signal.mailbox 0

getLevel : LevelId -> Task Http.Error Level
getLevel id = Http.get levelDecoder (levelUrl id)

maybeLevelId : GameState -> Maybe LevelId
maybeLevelId gameState =
    case gameState of
      LoadLevel id -> Just id
      _ -> Nothing

gameNewLevel : Signal LevelId
gameNewLevel = levelState
             |> Signal.filterMap maybeLevelId 0
             |> Signal.dropRepeats

port sendToQuery : Signal (Task LevelId ())
port sendToQuery =
    Signal.send levelQuery.address <~ gameNewLevel

{-- Main signals -------------------------------------------------------------
------------------------------------------------------------------------------}

delta : Signal Float
delta = Time.fps 30

loadComplete : Signal Input
loadComplete =
    Signal.sampleOn currentLevel.signal
          <| LevelLoaded <~ levelQuery.signal ~ currentLevel.signal

input : Signal Input
input = Signal.mergeMany [ (TimeDelta <~ delta)
                         , (UserAction <~ userInput)
                         , loadComplete]

levelState : Signal GameState
levelState =
    Signal.foldp stepGameState defaultGame input

main : Signal Element
main = display <~ Window.dimensions ~ levelState