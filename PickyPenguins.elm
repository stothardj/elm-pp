import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time
import Window
import Keyboard


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
userInput =
    Signal.map2 UserInput
          (Signal.map .x Keyboard.arrows)
          (Signal.map .y Keyboard.arrows)


type alias Input =
    { timeDelta : Float
    , userInput : UserInput
    }



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

type alias Positioned a = { a | x : Int, y : Int}

type alias Box = Positioned
    { color : GameColor
    , boxActions : List BoxAction}

type alias Goal = Positioned
    { color : GameColor}

type alias Wall = Positioned
    {}

type alias Direction =
    { dx : Int
    , dy : Int}

type alias Dimensions =
    { dimx : Int
    , dimy : Int}

type alias GameState =
    { boxes : List Box
    , goals : List Goal
    , walls : List Wall
    , direction : Direction
    , dimensions : Dimensions
    , frame : Int}

still : Direction
still = { dx = 0, dy = 0}

defaultGame : GameState
defaultGame =
    { boxes = [ {x = 3, y = 5, color = Red, boxActions = []}
              , {x = 1, y = 9, color = Blue, boxActions = []}
              , {x = 6, y = 6, color = Green, boxActions = []}
              , {x = 4, y = 8, color = Red, boxActions = []}
              , {x = 4, y = 6, color = Green, boxActions = []}]
    , goals = [ {x = 5, y = 5, color = Red}]
    , walls = [ {x = 3, y = 9}
              , {x = 1, y = 2}
              , {x = 6, y = 9}]
    , direction = still
    , dimensions = {dimx = 10, dimy = 12}
    , frame = numFrames - 1}


{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

numFrames = 5

occupiesSameSpot : Positioned a -> Positioned b -> Bool
occupiesSameSpot p1 p2 = p1.x == p2.x && p1.y == p2.y

inBounds : Dimensions -> Box -> Bool
inBounds {dimx,dimy} {x,y} =
    x >= 0 && x < dimx && y >= 0 && y < dimy

clearOfPositioned : List (Positioned a) -> Box -> Bool
clearOfPositioned xs box = xs
                 |> List.filter (occupiesSameSpot box)
                 |> List.isEmpty

reachedGoal : List Goal -> Box -> Bool
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

moveBox : Direction -> Box -> Box
moveBox {dx,dy} ({x,y} as box) =
    { box |
            x <- x + dx,
            y <- y + dy
    }

comp : (a -> Bool) -> a -> Bool
comp f x = not (f x)

determineActions : GameState -> GameState
determineActions gameState =
    { gameState | boxes <- List.map (tagBoxAction gameState) gameState.boxes }

applyActions : GameState -> GameState
applyActions gameState =
    let remaining = List.filter (comp (List.member BoxDisappear) << .boxActions) gameState.boxes
        mobile = List.filter (List.member BoxMove << .boxActions) remaining
        immobile = List.filter ((List.member BoxMove << .boxActions) |> comp) remaining
    in { gameState | boxes <- immobile ++ List.map (moveBox gameState.direction) mobile,
                     direction <- if List.isEmpty mobile then still else gameState.direction }

tryChangeDirection : UserInput -> GameState -> GameState
tryChangeDirection input gameState =
    if (gameState.direction == still)
      then { gameState | direction <- input }
      else gameState

stepGameObjects : Input -> GameState -> GameState
stepGameObjects {timeDelta,userInput} gameState =
    gameState
        |> applyActions
        |> tryChangeDirection userInput
        |> determineActions

stepFrame : GameState -> GameState
stepFrame gameState =
    if gameState.direction == still
    then gameState
    else { gameState | frame <- (gameState.frame + 1) % numFrames}

stepGame : Input -> GameState -> GameState
stepGame input gameState =
    (if gameState.frame == numFrames - 1 then stepGameObjects input gameState else gameState) |> stepFrame

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

display : (Int,Int) -> GameState -> Element
-- display (w,h) gameState =
--     show gameState
display (w,h) gameState = 
    let cellDim = cellDimensions gameState.dimensions
    in container w h middle <|
       collage gameWidth gameHeight
                   ([ gameDimensions |> irect |> filled backgroundColor]
                    ++ List.map (\ b -> displayBox cellDim b |> tweenBox cellDim b gameState) gameState.boxes
                    ++ List.map (displayWall cellDim) gameState.walls
                    ++ List.map (displayGoal cellDim) gameState.goals)



{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and shows it on screen.

------------------------------------------------------------------------------}

delta : Signal Float
delta =
    Time.fps 30


input : Signal Input
input =
    Signal.sampleOn delta (Signal.map2 Input delta userInput)


gameState : Signal GameState
gameState =
    Signal.foldp stepGame defaultGame input


main : Signal Element
main =
    Signal.map2 display Window.dimensions gameState