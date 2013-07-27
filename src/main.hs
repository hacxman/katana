{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns #-}
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

instance Num (Vector3 GLfloat) where
  (Vector3 a b c) + (Vector3 d e f) = Vector3 (a+d) (b+e) (c+f)

instance Num (GLfloat, Vector3 GLfloat) where
  (u, (Vector3 a b c)) + (v, (Vector3 d e f)) = (u+v, Vector3 (a+d) (b+e) (c+f))

width = 400 :: GLfloat
height = 400 :: GLfloat
maxslices = 100 :: Integer

nWatch ify fun fname = addWatch ify [Attrib] fname (
    \e -> case e         of
               Ignored   -> nWatch ify fun fname >> return ()
               otherwise -> print (fname, e) >> fun fname
    )

showAxes = do
  lineWidth $= 3
  lighting $= Disabled
  renderPrimitive Lines $ do
    currentColor $= Color4 1.0 0.0 (0.0::GLfloat) 1.0
    vertex $ Vertex3 0.0 0.0 (0.0::GLfloat)
    vertex $ Vertex3 10.0 0.0 (0.0::GLfloat)
    vertex $ Vertex3 9.0 1.0 (0.0::GLfloat)
    vertex $ Vertex3 10.0 0.0 (0.0::GLfloat)
    vertex $ Vertex3 9.0 (-1.0) (0.0::GLfloat)
    vertex $ Vertex3 10.0 0.0 (0.0::GLfloat)

  renderPrimitive Lines $ do
    currentColor $= Color4 0.0 1.0 (0.0::GLfloat) 1.0
    vertex $ Vertex3 0.0 0.0 (0.0::GLfloat)
    vertex $ Vertex3 0.0 10.0 (0.0::GLfloat)
    vertex $ Vertex3 1.0 9.0 (0.0::GLfloat)
    vertex $ Vertex3 0.0 10.0 (0.0::GLfloat)
    vertex $ Vertex3 (-1.0) 9.0 (0.0::GLfloat)
    vertex $ Vertex3 0.0 10.0 (0.0::GLfloat)

  renderPrimitive Lines $ do
    currentColor $= Color4 0.0 0.0 (1.0::GLfloat) 1.0
    vertex $ Vertex3 0.0 0.0 (0.0::GLfloat)
    vertex $ Vertex3 0.0 0.0 (10.0::GLfloat)
    vertex $ Vertex3 (1.0::GLfloat) 0.0 9.0
    vertex $ Vertex3 (0.0::GLfloat) 0.0 10.0
    vertex $ Vertex3 (-1.0::GLfloat) 0.0 9.0
    vertex $ Vertex3 (0.0::GLfloat) 0.0 10.0

  lineWidth $= 1
  lighting $= Enabled

idle = postRedisplay Nothing
points = [(a,b) | a <- [-1,(-1+2.0/width)..1.0], b <- [-1,(-1+2.0/height)..1.0]]
points' = points `using` parListChunk 256 rdeepseq

display ref model bounds rotA mov pos rot = do
    qq <- readIORef ref
    rotAx <- get rotA
    rot $~ (+rotAx)
    rotax <- get rot
    movax <- get mov
    pos $~ (+movax)
    posax <- get pos
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
    translate $ posax
    uncurry rotate rotax
  --  scale 0.5 0.5 (0.5 ::GLfloat)
--    rotate (fromIntegral qq) $ (Vector3 1.0 0.0 0.0 :: Vector3 GLfloat)
--    rotate (fromIntegral (qq+45)) $ (Vector3 0.0 1.0 0.0 :: Vector3 GLfloat)
--    rotate (fromIntegral (qq+75)) $ (Vector3 0.0 0.0 1.0 :: Vector3 GLfloat)
    model' <- get model
    preservingMatrix $ callList model'

    loadIdentity -- reset any transformation
    ortho (-100) 100 (-100) 100 (-100) 100
    matrixMode $= Modelview 0
    loadIdentity

    translate $ Vector3 30 (-30) (0::GLfloat)
    uncurry rotate rotax
--    rotate (fromIntegral qq) $ (Vector3 1.0 0.0 0.0 :: Vector3 GLfloat)
--    rotate (fromIntegral (qq+45)) $ (Vector3 0.0 1.0 0.0 :: Vector3 GLfloat)
--    rotate (fromIntegral (qq+75)) $ (Vector3 0.0 0.0 1.0 :: Vector3 GLfloat)
    preservingMatrix $ showAxes
    swapBuffers -- refresh screen

rot_args = [("-r", ( 90.0, Vector3   0.0   1.0   0.0 :: Vector3 GLfloat)),
            ("-l", ( 90.0, Vector3   0.0 (-1.0)  0.0)),
            ("-b", ( 90.0, Vector3   1.0   0.0   0.0)),
            ("-t", ( 90.0, Vector3 (-1.0)  0.0   0.0)),
            ("-k", (180.0, Vector3   1.0   0.0   0.0)),
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
      currentColor $= (Color4 1.0 0.4 0.7 1.0 :: Color4 GLfloat)
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

  multisample $= Enabled
  g <- get sampleBuffers
  print g
--  liftM print $ get samples

  position (Light 0) $= Vertex4 10000 10000 10000 0
  ambient (Light 0) $= Color4 1 1 1 1

  light (Light 0) $= Enabled
  lighting $= Enabled
  depthFunc $= Just Less
  normalize $= Enabled
  la <- newIORef (0 :: Integer)
  rot <- newIORef (maybe (0,Vector3 0 0 0) id $ lookup (args!!1) rot_args)
  rotA <- newIORef (0,Vector3 0 0 0)
  mov <- newIORef (Vector3 0.0 0.0 (0.0 :: GLfloat))
  pos <- newIORef (Vector3 0.0 0.0 (0.0 :: GLfloat))

  idleCallback $= Just idle
  displayCallback $= display la mlist model_size rotA mov pos rot
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
--  colorMaterial $= Enabled
  colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  keyboardMouseCallback $= Just (keyboard rotA mov)
  shadeModel $= Smooth

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
--    keyboard rot mov (SpecialKey KeyHome    ) Up   (shift -> Up)   _ = rot $~ (+(0,Vector3 0.0 (-1.0) 0.0))
--    keyboard rot mov (SpecialKey KeyHome    ) Down (shift -> Down) _ = rot $~ (+(2,Vector3 0.0 (1.0) 0.0))
--    keyboard rot mov (SpecialKey KeyEnd     ) Up   (shift -> Up)   _ = rot $~ (+(0,Vector3 0.0 (1.0) 0.0))
--    keyboard rot mov (SpecialKey KeyEnd     ) Down (shift -> Down) _ = rot $~ (+(2,Vector3 0.0 (-1.0) 0.0))
--    keyboard rot mov (SpecialKey KeyDelete  ) Up   (shift -> Up)   _ = rot $~ (+(0,Vector3 (1.0) 0.0 0.0))
--    keyboard rot mov (SpecialKey KeyDelete  ) Down (shift -> Down) _ = rot $~ (+(2,Vector3 (-1.0) (0.0) 0.0))
--    keyboard rot mov (SpecialKey KeyPageDown) Up   (shift -> Up)   _ = rot $~ (+(0,Vector3 (-1.0) 0.0 0.0))
--    keyboard rot mov (SpecialKey KeyPageDown) Down (shift -> Down) _ = rot $~ (+(2,Vector3 (1.0) 0.0 0.0))

    keyboard rot mov (SpecialKey KeyHome    ) Up   _ _ = rot $~ (+(-1,Vector3  0.0  0.0  (1.0)))
    keyboard rot mov (SpecialKey KeyHome    ) Down _ _ = rot $~ (+( 1,Vector3  0.0  0.0  (1.0)))
    keyboard rot mov (SpecialKey KeyEnd     ) Up   _ _ = rot $~ (+( 1,Vector3  0.0  0.0  (1.0)))
    keyboard rot mov (SpecialKey KeyEnd     ) Down _ _ = rot $~ (+(-1,Vector3  0.0  0.0  (1.0)))
    keyboard rot mov (SpecialKey KeyDelete  ) Up   _ _ = rot $~ (+( 1,Vector3 (1.0)  0.0  0.0))
    keyboard rot mov (SpecialKey KeyDelete  ) Down _ _ = rot $~ (+(-1,Vector3 (1.0) (0.0) 0.0))
    keyboard rot mov (SpecialKey KeyPageDown) Up   _ _ = rot $~ (+(-1,Vector3 (1.0)  0.0  0.0))
    keyboard rot mov (SpecialKey KeyPageDown) Down _ _ = rot $~ (+( 1,Vector3 (1.0)  0.0  0.0))

    keyboard rot mov (SpecialKey KeyUp   ) Up   _ _ = mov $~ (+Vector3 0.0 (-1.0) 0.0)
    keyboard rot mov (SpecialKey KeyUp   ) Down _ _ = mov $~ (+Vector3 0.0 (1.0) 0.0)
    keyboard rot mov (SpecialKey KeyDown ) Up   _ _ = mov $~ (+Vector3 0.0 (1.0) 0.0)
    keyboard rot mov (SpecialKey KeyDown ) Down _ _ = mov $~ (+Vector3 0.0 (-1.0) 0.0)
    keyboard rot mov (SpecialKey KeyLeft ) Up   _ _ = mov $~ (+Vector3 (1.0) 0.0 0.0)
    keyboard rot mov (SpecialKey KeyLeft ) Down _ _ = mov $~ (+Vector3 (-1.0) (0.0) 0.0)
    keyboard rot mov (SpecialKey KeyRight) Up   _ _ = mov $~ (+Vector3 (-1.0) 0.0 0.0)
    keyboard rot mov (SpecialKey KeyRight) Down _ _ = mov $~ (+Vector3 (1.0) 0.0 0.0)
    keyboard rot mov (Char c) _ _ _ =
      case lookup ('-':[c]) rot_args of
        Just r  -> do rot $= r
        Nothing -> return ()
    kyeboard _ _ _ _ _ _ = return ()
