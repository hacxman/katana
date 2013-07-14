{-# LANGUAGE TypeSynonymInstances #-}
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad
import STL

width = 400 :: GLfloat
height = 400 :: GLfloat
maxslices = 1000 :: Integer

idle = postRedisplay Nothing
points = [(a,b) | a <- [-1,(-1+2.0/width)..1.0], b <- [-1,(-1+2.0/height)..1.0]]
points' = points `using` parListChunk 256 rdeepseq

display ref model bounds = do
  qq <- readIORef ref
  putStr $ (show qq)++"\n"
  modifyIORef ref (+1)

  clearColor $= Color4 0 0.1686 0.2117 1
  clear [ColorBuffer,DepthBuffer]
  matrixMode $= Projection
  -- Transformation to change the view
  loadIdentity -- reset any transformation
  ortho (-bounds) bounds (-bounds) bounds (0.1) bounds
  matrixMode $= Modelview 0
  loadIdentity

  translate $ (Vector3 0 0 (realToFrac $ -bounds/2) :: Vector3 GLfloat)
--  scale 0.5 0.5 (0.5 ::GLfloat)
  rotate (fromIntegral qq) $ (Vector3 1.0 0.0 0.0 :: Vector3 GLfloat)
  preservingMatrix $ callList model

  swapBuffers -- refresh screen

p2v (STL.Point a b c) = Vertex3 (realToFrac a)
                                (realToFrac b)
                                (realToFrac c) :: Vertex3 GLfloat
p2n (STL.Point a b c) = Normal3 (realToFrac a)
                                (realToFrac b)
                                (realToFrac c) :: Normal3 GLfloat
main = do
  (prognam,args) <- getArgsAndInitialize
  initialDisplayMode $=
      [WithDepthBuffer,DoubleBuffered,RGBAMode,WithAlphaComponent]
-- We create a window with some title
  createWindow "katana"
-- We add some directives
  depthFunc  $= Just Less
  windowSize $= Size (round width) (round height)

--  STL n facets <- loadSTL "32ce64c4-df76-11e2-9db0-5404a640b18f.stl"
  print args
  STL n facets <- loadSTL $ args!!0
  let model = map (\(Facet n x y z) -> (mapp x, mapp y, mapp z)) facets
      mapp (STL.Point a b c) = Vertex3 (realToFrac a) 
                                       (realToFrac b)
                                       (realToFrac c) :: Vertex3 GLfloat
      (aa', bb', cc') = unzip3 model
      (xx, yy, zz) = unzip3 $ map (\(Vertex3 q w e) -> (q,w,e)) (aa'++bb'++cc')
      
      bounding_box = (maximum xx, minimum xx,
                      maximum yy, minimum yy,
                      maximum zz, minimum zz)
      bounds (a,b,c,d,e,f) = realToFrac $ maximum [abs(a-b), abs(c-d), abs(e-f)] :: GLdouble

  mlist <- defineNewList Compile $ do
    renderPrimitive Triangles $
      mapM_ (\(Facet n x y z) -> do
      color $ (Color3 0.5 0.4 0.7 :: Color3 GLfloat)
      normal $ p2n n
      vertex $ p2v x
      vertex $ p2v y
      vertex $ p2v z) facets

  position (Light 0) $= Vertex4 1 1 1 0

  light (Light 0) $= Enabled
  lighting $= Enabled
  depthFunc $= Just Less

  la <- newIORef (0 :: Integer)

  idleCallback $= Just idle
  displayCallback $= display la mlist (bounds bounding_box)
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
-- We enter the main loop
  mainLoop
