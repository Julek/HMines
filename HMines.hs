import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Control.Monad.Trans
import Data.Array.IO
import Data.IORef
import Data.Ix
import Data.List
import Data.List.Split
import Data.Maybe
import Graphics.UI.Gtk hiding (eventSent, eventButton)
import Graphics.UI.Gtk.Gdk.Events
import System.Random.Shuffle

data Board = Board (Int, Int) [(Int, Int)] [(Int, Int)] [(Int, Int)]
data Status = Win | Loose | Playing
     deriving(Show, Eq)

main :: IO ()
main = do
      let x = 10
          y = 10
          numMines = 10
      x <- untilJust (putStrLn "x size of board: " >> fmap readMaybe getLine) :: IO Int
      y <- untilJust (putStrLn "y size of board: " >> fmap readMaybe getLine) :: IO Int
      numMines <- untilJust (putStrLn "number of mines in board: " >> fmap readMaybe getLine >>= \mRet -> return $ (mRet >>= \ret -> if ret < x * y then Just ret else Nothing)) :: IO Int
      board@(Board _ _ mines _) <- createBoard (x, y) numMines

      initGUI -- startup the GUI
      
      window <- windowNew -- create Window
      set window [windowTitle := "Minesweeper"]
      on window deleteEvent (liftIO mainQuit >> return False) -- when window killed die.
      

      table <- tableNew x y True
      buttonArray <- newListArray ((0, 0), (x - 1, y -1)) (replicate (x * y) undefined) :: IO (IOArray (Int, Int) Button)
      stateRef <- newIORef (buttonArray, board, Playing)
      buttons <- mapM_ (\c@(x,y) -> do
                       button <- buttonNewWithLabel ""
                       onButtonPress button (onClick c stateRef)
                       buttonSetFocusOnClick button False
                       writeArray buttonArray c button
                       tableAttachDefaults table button x (x+1) y (y+1)
                       ) (range ((0, 0), (x-1, y-1)))      
      containerAdd window table

      widgetShowAll window
      mainGUI
      (_, _, f) <- readIORef stateRef
      when (f /= Playing) (print f)

createBoard :: (Int, Int) -> Int -> IO Board
createBoard (x, y) numMines = do
                            mines <- fmap (take numMines) . shuffleM . range $ ((0, 0), (x - 1, y - 1))
                            return (Board (x,y) [] mines [])

onClick :: (Int, Int) -> IORef (IOArray (Int, Int) Button, Board, Status) -> Event -> IO Bool
onClick c ref event = (case eventButton event of
                           LeftButton -> onLeftClick c ref
                           RightButton -> onRightClick c ref) >> return (eventSent event)

onLeftClick :: (Int, Int) -> IORef (IOArray (Int, Int) Button, Board, Status) -> IO ()
onLeftClick c@(x,y) ref = do
                  (buttonArray, board@(Board sz@(xSize, ySize) cleared mines flaged), _) <- readIORef ref
                  (nBoard@(Board _ nCleared _ _), res) <- if elem c mines
                                                          then do
                                                             mainQuit
                                                             return (Board sz (c:cleared) mines flaged, Loose)
                                                         else do
                                                              let 
                                                                  b@(Board _ cleared' _ _) = 
                                                                           if elem c cleared
                                                                           then if (numberSurrounding c board) == (length $ intersect flaged (getSurrounding c board))
                                                                                then Board sz  (nub $ ((getSurrounding c board) \\ flaged) ++ cleared) mines flaged
                                                                                else Board sz cleared mines flaged
                                                                           else updateBoard c board
                                                              if null $ intersect cleared' mines
                                                              then
                                                                if null (((range ((0,0), (xSize - 1, ySize - 1))) \\ cleared') \\ mines)
                                                                then do
                                                                   mainQuit
                                                                   return (Board sz cleared' mines flaged, Win)
                                                                else return (Board sz cleared' mines flaged, Playing)
                                                              else return (Board sz cleared' mines flaged, Loose)
                  redraw buttonArray nBoard
                  writeIORef ref (buttonArray, nBoard, res)

onRightClick :: (Int, Int) -> IORef (IOArray (Int, Int) Button, Board, Status) -> IO ()
onRightClick c ref = do
             (buttonArray, board@(Board sz cleared mines flaged), stat) <- readIORef ref
             let board' = if elem c flaged
                           then  Board sz cleared mines (delete c flaged)
                           else if elem c cleared
                                then Board sz cleared mines flaged
                                else Board sz cleared mines (c:flaged)
             redraw buttonArray board'
             writeIORef ref (buttonArray, board', stat)
             
redraw :: IOArray (Int, Int) Button -> Board -> IO ()
redraw buttonArray board@(Board (xSize, ySize) cleared mines flaged) = do
                   mapM_ (\cell -> do
                                button <- readArray buttonArray cell
                                buttonSetLabel button ""
                         ) (range ((0, 0), (xSize - 1, ySize - 1)))
                   mapM_ (\cell -> do
                              button <- readArray buttonArray cell
                              let label = numberSurrounding cell board
                              buttonSetRelief button ReliefNone
                              if label == 0
                              then buttonSetLabel button ""
                              else buttonSetLabel button . show $ label
                        ) cleared
{-                   mapM_ (\cell -> do
                              button <- readArray buttonArray cell
                              buttonSetLabel button "!!!"
                         ) mines-}
                   mapM_ (\cell -> do
                              button <- readArray buttonArray cell
                              buttonSetLabel button "!"
                         ) flaged
             

updateBoard :: (Int, Int) -> Board -> Board
updateBoard c board@(Board size cleared mines flaged) = Board size (nub $ cleared ++ ncleared) mines flaged
                                where ncleared = updateBoard' board [c]
                                  
updateBoard' :: Board -> [(Int, Int)] -> [(Int, Int)]
updateBoard' board@(Board _ cleared mines flaged) clearing = if null $ nclearing
                                                      then (nub $ concatMap (\c -> c:getSurrounding c board) $ clearing) \\ occupied
                                                      else updateBoard' board (clearing ++ nclearing)
             where nclearing = (nub . filter (\c -> (numberSurrounding c board == 0) && (not . elem c $ mines)) $ (concatMap (\c -> getSurrounding c board) clearing)) \\ (occupied ++ clearing)
                   occupied = cleared ++ mines ++ flaged

numberSurrounding :: (Int, Int) -> Board -> Int
numberSurrounding (x,y) board@(Board _ _ mines _) = length . intersect mines $ getSurrounding (x,y) board

getSurrounding :: (Int, Int) -> Board -> [(Int, Int)]
getSurrounding c@(x,y) (Board (xSize, ySize) _ _ _) = [(x + dx, y + dy) | dx <- [-1,0,1], dy <- [-1,0,1], x + dx >= 0, x + dx < xSize, y + dy >= 0, y + dy < ySize, (dx /= 0 || dy /= 0)]

readMaybe :: (Read a) => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads
