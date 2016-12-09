{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex
import Reflex.Dom
import Control.Monad.Random (RandomGen, Rand, runRand, getStdGen, getRandomR)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (State, state, runState)
import Data.Map as DM (Map, fromList, elems, lookup, findWithDefault, insert, mapWithKey, (!))
import Data.Text (Text, pack)
import Data.Functor.Misc (dmapToMap, mapWithFunctorToDMap)
import Data.Traversable (forM)

data Cell = Cell { mined :: Bool 
                 , exposed :: Bool
                 , flagged :: Bool
                 , mines :: Int
                 } deriving Show

type Pos = (Int, Int)
type Board = Map Pos Cell

data Msg = RightPick Pos 

w :: Int
w =  20

h :: Int
h = 10

cellSize :: Int
cellSize = 20

mkCell :: RandomGen g => Rand g Cell
mkCell = do
    t <- getRandomR (0.0::Float, 1.0)
    return $ Cell (t < 0.201) False False 0

initBoard :: RandomGen g => [Pos] -> Rand g Board
initBoard positions = do
    cells <- sequence $ repeat mkCell
    return $ fromList (zip positions cells)

mkBoard :: RandomGen g => Rand g Board
mkBoard = do
    let positions = [(x,y) | x <- [0..w-1], y <- [0..h-1]]   
    initBoard positions

getColor :: Cell -> String
getColor (Cell _ exposed _ _) = if exposed then "#909090" else "#AAAAAA"

cellAttrs :: Cell -> Map Text Text
cellAttrs cell = 
    let size = 0.9
        placement = 0.5 - (size/2.0)

    in fromList [ ( "x",            pack $ show placement)
                , ( "y",            pack $ show placement)
                , ( "width",        pack $ show size)
                , ( "height",       pack $ show size)
                , ( "style",        pack $ "fill:" ++ getColor cell)
                , ("oncontextmenu", "return false;")
                ] 

groupAttrs :: Pos -> Map Text Text
groupAttrs (x,y) = 
    fromList [ ("transform", 
                pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                       ++ "translate (" ++ show x ++ ", " ++ show y ++ ")" 
               )
             ] 

showSquare :: MonadWidget t m => Cell -> m [El t]
showSquare c = do
    (rEl,_) <- elSvgns "rect" (constDyn $ cellAttrs c) $ return ()
    return [rEl]

showFlag :: MonadWidget t m => Pos -> m [El t]
showFlag pos = do
    let flagAttrs = 
            fromList [ ( "points", "0.20,0.40 0.70,0.55 0.70,0.25" )
                     , ( "style",        "fill:red")
                     , ("oncontextmenu", "return false;")
                     ] 

    (fEl,_) <- elSvgns "polygon" (constDyn flagAttrs ) $ return ()

    let poleAttrs = 
            fromList [ ( "x1", "0.70" )
                     , ( "y1", "0.25" )
                     , ( "x2", "0.70" )
                     , ( "y2", "0.85" )
                     , ( "stroke-width", ".07")
                     , ( "stroke", "black")
                     , ("oncontextmenu", "return false;")
                     ] 

    (pEl,_) <- elSvgns "line" (constDyn poleAttrs ) $ return ()

    return [fEl, pEl]

showCellDetail :: MonadWidget t m => Pos -> Cell -> m [El t]
showCellDetail pos c@(Cell mined exposed flagged mines) = 
    case (  mined, exposed, flagged, 0 == mines) of
         (      _,       _,    True,     _) -> showFlag pos 
         (      _,       _,       _,     _) -> return []

mouseEv :: Reflex t => Pos -> El t -> [Event t Msg]
mouseEv pos el = 
    let r_rEv = RightPick pos <$ domEvent Contextmenu el
    in [r_rEv]

showCell :: MonadWidget t m => Pos -> Cell -> m (Event t Msg)
showCell pos c = 
    fmap snd $ elSvgns "g"  (constDyn $ groupAttrs pos) $ do
        rEl <- showSquare c
        dEl <- showCellDetail pos c 
        return $ leftmost $ concatMap (mouseEv pos) (rEl ++ dEl)

showAndReturnCell :: MonadWidget t m => Pos -> Cell -> m (Event t Msg, Cell)
showAndReturnCell pos c = do
    ev <- showCell pos c
    return (ev,c)

fromPick :: Msg -> Board ->[(Pos, Maybe Cell)]
fromPick (RightPick pos ) board = 
    let c = board ! pos
    in if exposed c
       then [] -- can't flag a cell that's already exposed.
       else [(pos, Just c {flagged=not $ flagged c})]

reactToPick :: (Board,Msg) -> Map Pos (Maybe Cell)
reactToPick (b,c) = fromList $ fromPick c b

boardAttrs :: Map Text Text
boardAttrs = fromList 
                 [ ("width" , pack $ show $ w * cellSize)
                 , ("height", pack $ show $ h * cellSize)
                 , ("style" , "border:solid; margin:8em")
                 , ("oncontextmenu", "return false;")
                 ]

showCell2 :: forall t m. MonadWidget t m => Dynamic t (Map Pos Cell) -> Pos -> m (Event t Msg)
showCell2 dBoard pos = do
    let dCell = fmap (findWithDefault (Cell False False False 0) pos) dBoard
    ev2 :: Event t (Event t Msg) <- dyn (fmap (showCell pos) dCell)
    ev3 :: Behavior t (Event t Msg) <- hold never ev2
    let ev4 :: Event t Msg = switch ev3
    return ev4

updateBoard :: Msg -> Board -> Board
updateBoard msg oldBoard = 
        let updates = fromPick msg oldBoard 
            updater b (p, Just c) = insert p c b
        in foldl updater oldBoard updates

showBoard2 :: forall t m. MonadWidget t m => m ()
showBoard2 = do 
    gen <- liftIO getStdGen
    rec 
        let indices = [(x,y) | x <- [0..w-1], y <- [0..h-1]] 
            (initialBoard, _)  = runRand (initBoard indices) gen
        board <- foldDyn updateBoard initialBoard (leftmost ev)
        (_, ev) <- elSvgns "svg" (constDyn boardAttrs) $ forM indices $ showCell2 board
    return ()

showBoard :: MonadWidget t m => m ()
showBoard = do
    gen <- liftIO getStdGen
    let (initial, _)  = runRand mkBoard gen
    rec 
        let pick = switch $ (leftmost . elems) <$> current ev
            pickWithCells = attachPromptlyDynWith (,) cm pick
            updateEv = fmap reactToPick pickWithCells
            eventAndCellMap = listHoldWithKey initial updateEv showAndReturnCell 
            eventMap = fmap (fmap (fmap fst)) eventAndCellMap
        (_, ev) <- elSvgns "svg" (constDyn boardAttrs) eventMap
        let cellMap = fmap (fmap (fmap snd)) eventAndCellMap
        cm <- cellMap 
    return ()

main :: IO ()
main = mainWidget showBoard2

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")
