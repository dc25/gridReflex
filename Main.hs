{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex
import Reflex.Dom
import Data.Map as DM (Map, fromList, elems, findWithDefault, insert , (!))
import Data.Text (Text, pack)
import Data.Traversable (forM)

type Pos = (Int, Int)
type Board = Map Pos Bool

data Msg = Pick Pos Bool

w :: Int
w =  20

h :: Int
h = 10

cellSize = 20

indices = [(x,y) | x <- [0..w-1], y <- [0..h-1]] 

initBoard :: [Pos] -> Board
initBoard positions = 
    let cells = repeat False
    in fromList (zip positions cells)

cellAttrs :: Bool -> Map Text Text
cellAttrs flagged = 
    let size = 0.9
        placement = 0.5 - (size/2.0)

    in fromList [ ( "x",            pack $ show placement)
                , ( "y",            pack $ show placement)
                , ( "width",        pack $ show size)
                , ( "height",       pack $ show size)
                , ( "style",        pack $ "fill:" ++ (if flagged then "grey" else "blue"))
                ] 

groupAttrs :: Pos -> Map Text Text
groupAttrs (x,y) = 
    fromList [ ("transform", 
                pack $    "scale (" ++ show cellSize ++ ", " ++ show cellSize ++ ") " 
                       ++ "translate (" ++ show x ++ ", " ++ show y ++ ")" 
               )
             ] 

showCell :: MonadWidget t m => Pos -> Bool -> m (Event t Msg)
showCell pos flagged = 
    fmap snd $ elSvgns "g"  (constDyn $ groupAttrs pos) $ do
        (rEl,_) <- elSvgns "rect" (constDyn $ cellAttrs flagged) $ return ()
        return $ Pick pos flagged <$ domEvent Click rEl

fromPick :: Msg -> [(Pos, Maybe Bool)]
fromPick (Pick pos flagged) = [(pos, Just $ not flagged )]

reactToPick :: Msg -> Map Pos (Maybe Bool)
reactToPick c = fromList $ fromPick c 

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
        let updates = fromPick msg 
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
            updateEv = fmap reactToPick pick
            eventMap = listHoldWithKey initial updateEv showCell
        (_, ev) <- elSvgns "svg" (constDyn boardAttrs) eventMap
    return ()

main :: IO ()
main = mainWidget showBoard2

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")
