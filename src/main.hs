{-# LANGUAGE TypeSynonymInstances #-}
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad
import STL
import System.INotify
import Control.Concurrent.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Data.Maybe

--data ProgState = ProgState {
--    position :: Vertex3 GLfloat
--  }

width = 400 :: GLfloat
height = 400 :: GLfloat
maxslices = 100 :: Integer

nWatch ify fun fname = addWatch ify [Attrib] fname (
    \e -> case e         of
               Ignored   -> nWatch ify fun fname >> return ()
               otherwise -> print (fname, e) >> fun fname
    )


idle = postRedisplay Nothing
points = [(a,b) | a <- [-1,(-1+2.0/width)..1.0], b <- [-1,(-1+2.0/height)..1.0]]
points' = points `using` parListChunk 256 rdeepseq

display ref model bounds rotax = do
    qq <- readIORef ref
  --if qq > maxslices then error "uha"
  --else do
--    putStr $ (show qq)++"\n"
    modifyIORef ref (+1)

    clearColor $= Color4 0 0.1686 0.2117 1
    clear [ColorBuffer,DepthBuffer]
    matrixMode $= Projection
    -- Transformation to change the view
    loadIdentity -- reset any transformation
    ortho (-bounds) bounds (-bounds) bounds (-bounds) bounds
    matrixMode $= Modelview 0
    loadIdentity

    translate $ (Vector3 0 0 (realToFrac $ -bounds/2) :: Vector3 GLfloat)
    uncurry rotate rotax
  --  scale 0.5 0.5 (0.5 ::GLfloat)
    rotate (fromIntegral qq) $ (Vector3 1.0 0.0 0.0 :: Vector3 GLfloat)
    rotate (fromIntegral (qq+45)) $ (Vector3 0.0 1.0 0.0 :: Vector3 GLfloat)
    rotate (fromIntegral (qq+75)) $ (Vector3 0.0 0.0 1.0 :: Vector3 GLfloat)
    model' <- get model
    preservingMatrix $ callList model'

    swapBuffers -- refresh screen

rot_args = [("-r", ( 90.0, Vector3   0.0   1.0   0.0 :: Vector3 GLfloat)),
            ("-l", ( 90.0, Vector3   0.0 (-1.0)  0.0)),
            ("-b", ( 90.0, Vector3   1.0   0.0   0.0)),
            ("-t", ( 90.0, Vector3 (-1.0)  0.0   0.0)),
            ("-bk",(180.0, Vector3   1.0   0.0   0.0)),
            ("-f", (180.0, Vector3 (-1.0)  0.0   0.0))]

modelSize facets = model_size
  where model = map (\(Facet n x y z) -> (x, y, z)) facets
        (aa', bb', cc') = unzip3 model
        (xx, yy, zz) = unzip3 $ map (\(Vertex3 q w e) -> (q,w,e)) (aa'++bb'++cc')
        bounding_box = (maximum xx, minimum xx,
                        maximum yy, minimum yy,
                        maximum zz, minimum zz)
        model_size = bounds bounding_box
        bounds (a,b,c,d,e,f) = realToFrac $
          maximum [abs(a-b), abs(c-d), abs(e-f)] :: GLdouble

displistFromFacets facets = do
  mlist' <- defineNewList Compile $ do
    renderPrimitive Triangles $
      mapM_ (\(Facet n x y z) -> do
      color $ (Color3 0.5 0.4 0.7 :: Color3 GLfloat)
      normal $ (\(Vertex3 a b c) -> Normal3 a b c) n
      vertex $ x
      vertex $ y
      vertex $ z) facets
  return mlist'

main = do
  (prognam,args) <- getArgsAndInitialize
  initialDisplayMode $=
      [WithDepthBuffer,DoubleBuffered,RGBAMode,WithAlphaComponent]
-- We create a window with some title
  createWindow "katana"
-- We add some directives
  depthFunc  $= Just Less
  windowSize $= Size (round width) (round height)

  print args
  STL n facets <- loadSTL $ args!!0

  let model_size = modelSize facets
  print model_size

  mlist' <- displistFromFacets facets
  mlist <- newIORef mlist'

  position (Light 0) $= Vertex4 1 1 1 0

  light (Light 0) $= Enabled
  lighting $= Enabled
  depthFunc $= Just Less

  la <- newIORef (0 :: Integer)

  idleCallback $= Just idle
  displayCallback $= display la mlist model_size (fromJust $ lookup (args!!1) rot_args)
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  keyboardMouseCallback $= Just (keyboard pPos)

  ify <- initINotify
  w0 <- nWatch ify (reloader mlist) $ args!!0
--    \fname -> (forkIO $ reloader mlist fname) >> (return ())) $ args!!0

-- We enter the main loop
  mainLoop
  where
    reloader mlist fname = do
      STL n facets <- loadSTL fname
      displist <- displistFromFacets facets
      print $ modelSize facets
      finish
      lst <- get mlist
      deleteObjectNames [lst]
      mlist $= displist
      print ("FUHA", n)
