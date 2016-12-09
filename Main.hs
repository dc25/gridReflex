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

type Pos = (Int, Int)
type Board = Map Pos Bool

data Msg = Pick Pos 

w :: Int
w =  20

h :: Int
h = 10

indices = [(x,y) | x <- [0..w-1], y <- [0..h-1]] 

cellSize :: Int
cellSize = 20

initBoard :: [Pos] -> Board
initBoard positions = 
    let cells = repeat False
    in fromList (zip positions cells)

getColor :: Bool -> String
getColor flagged = if flagged then "#909090" else "#AAAAAA"

cellAttrs :: Bool -> Map Text Text
cellAttrs cell = 
    let size = 0.9
        placement = 0.5 - (size/2.0)

    in fromList [ ( "x",            pack $ show placement)
                , ( "y",            pack $ show placement)
                , ( "width",        pack $ show size)
                , ( "height",       pack $ show size)
                , ( "style",        pack $ "fill:" ++ getColor cell)
                ] 

groupAttrs :: Pos -> Map Text Text
groupAttrs (x,y) = 
    fromList [ ("transform", 
                pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                       ++ "translate (" ++ show x ++ ", " ++ show y ++ ")" 
               )
             ] 

showFlag :: MonadWidget t m => Pos -> m [El t]
showFlag pos = do
    let flagAttrs = 
            fromList [ ( "points", "0.20,0.40 0.70,0.55 0.70,0.25" )
                     , ( "style",        "fill:red")
                     ] 

    (fEl,_) <- elSvgns "polygon" (constDyn flagAttrs ) $ return ()

    return [fEl]

showCell :: MonadWidget t m => Pos -> Bool -> m (Event t Msg)
showCell pos flagged = 
    fmap snd $ elSvgns "g"  (constDyn $ groupAttrs pos) $ do
        (rEl,_) <- elSvgns "rect" (constDyn $ cellAttrs flagged) $ return ()
        if flagged then showFlag pos else return []
        return $ Pick pos <$ domEvent Click rEl

showAndReturnCell :: MonadWidget t m => Pos -> Bool -> m (Event t Msg, Bool)
showAndReturnCell pos c = do
    ev <- showCell pos c
    return (ev,c)

fromPick :: Msg -> Board ->[(Pos, Maybe Bool)]
fromPick (Pick pos ) board = 
    let flagged = board ! pos
    in [(pos, Just $ not flagged )]

reactToPick :: (Board,Msg) -> Map Pos (Maybe Bool)
reactToPick (b,c) = fromList $ fromPick c b

boardAttrs :: Map Text Text
boardAttrs = fromList 
                 [ ("width" , pack $ show $ w * cellSize)
                 , ("height", pack $ show $ h * cellSize)
                 , ("style" , "border:solid; margin:8em")
                 ]

showCell2 :: forall t m. MonadWidget t m => Dynamic t (Map Pos Bool) -> Pos -> m (Event t Msg)
showCell2 dBoard pos = do
    let dCell = fmap (findWithDefault False pos) dBoard
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
    rec 
        let  initial  = initBoard indices
        board <- foldDyn updateBoard initial (leftmost ev)
        (_, ev) <- elSvgns "svg" (constDyn boardAttrs) $ forM indices $ showCell2 board
    return ()

showBoard :: MonadWidget t m => m ()
showBoard = do
    let initial = initBoard [(x,y) | x <- [0..w-1], y <- [0..h-1]]   
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
