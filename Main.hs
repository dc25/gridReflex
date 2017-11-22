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

data Msg = Pick Pos Bool deriving Show

w :: Int
w =  30

h :: Int
h = 20

cellSize = 20

indices = [(x,y) | x <- [0..w-1], y <- [0..h-1]] 


initBoard :: [Pos] -> Board
initBoard positions = 
    let cells = repeat False
    in fromList (zip positions cells)

initialBoard  = initBoard indices

cellAttrs :: Bool -> Map Text Text
cellAttrs c =
    fromList [ ( "x",            "0.05")
             , ( "y",            "0.05")
             , ( "width",        "0.9")
             , ( "height",       "0.9")
             , ( "style",        pack $ "fill:" ++ if c then
 "red" else "orange")
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
    let dFlagged = fmap (findWithDefault False pos) dBoard
    bh <- hold never =<< dyn (fmap (showCell pos) dFlagged)
    return $ switch bh

showCell2b :: forall t m. MonadWidget t m => Dynamic t (Map Pos Bool) -> Pos -> m (Event t Msg)
showCell2b dBoard pos = do
    let dFlagged = fmap (findWithDefault False pos) dBoard
    fmap snd $ elSvgns "g"  (constDyn $ groupAttrs pos) $ do
        (rEl,_) <- elSvgns "rect" (fmap cellAttrs dFlagged) $ return ()
        flagged <- sample $ current dFlagged
        return $ Pick pos flagged <$ domEvent Click rEl

showCell2c :: forall t m. MonadWidget t m => Dynamic t (Map Pos Bool) -> Pos -> m (Event t Msg)
showCell2c dBoard pos = do
    let dFlagged = fmap (findWithDefault False pos) dBoard
    (el, _) <- elSvgns "g"  (constDyn $ groupAttrs pos) $ 
                   elSvgns "rect" (fmap cellAttrs dFlagged) $ return ()
    flagged <- sample $ current dFlagged
    return $ Pick pos flagged <$ domEvent Click el 

updateBoard :: Msg -> Board -> Board
updateBoard msg oldBoard = 
    let updates = fromPick msg
        updater b (p, Just c) = insert p c b
    in foldl updater oldBoard updates

autoPickButton :: MonadWidget t m => m (Event t Msg)
autoPickButton = do
    let autoPicks = zipWith (\f p -> f p False) (repeat Pick) [(x,y) | x <- [2,4..w-1], y <- [2,4..h -1]]
    m_bEv <- el "div" $ button "Autopick!!!" 
    zipListWithEvent const autoPicks m_bEv

showBoard2 :: forall t m. MonadWidget t m => m ()
showBoard2 = do 
    -- pick <- autoPickButton
    rec 
        let pick = leftmost ev
        board <- foldDyn updateBoard initialBoard pick
        (_, ev) <- elSvgns "svg" (constDyn boardAttrs) $ 
                       forM indices $ showCell2c board
    return ()

showBoard :: MonadWidget t m => m ()
showBoard = do
    -- pick <- autoPickButton
    rec 
        let 
            pick = switch $ (leftmost . elems) <$> current ev
            updateEv = fmap reactToPick pick
            eventMap = listHoldWithKey initialBoard updateEv showCell
        (_, ev) <- elSvgns "svg" (constDyn boardAttrs) eventMap
    return ()

main :: IO ()
main = mainWidget 
           showBoard2
           -- showBoard

elSvgns :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")
