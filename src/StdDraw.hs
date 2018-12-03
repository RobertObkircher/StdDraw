{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}

module StdDraw where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.STM    (TChan, atomically, isEmptyTChan,
                                            newTChanIO, readTChan, tryReadTChan,
                                            writeTChan)
import           Control.Exception         (finally)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, asks,
                                            runReaderT, when)
import           Control.Monad.State       (MonadState, StateT, get, gets,
                                            modify, put, runStateT)
import           Data.Default
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import           StdDraw.Colors
import           StdDraw.Util
import           System.IO                 (BufferMode (LineBuffering),
                                            hSetBuffering, stdout)

type Color = GL.Color3 GL.GLubyte

data KeyEvent = KeyUp GLFW.Key | KeyDown GLFW.Key

data DrawConfig = DrawConfig
  { defaultPenColor   :: Color
  , defaultClearColor :: Color
  , defaultSize       :: Int
  , defaultPenRadius  :: Double
  , border            :: Double -- boundary of drawing canvas, 0% border
  , defaultXMin       :: Double
  , defaultXMax       :: Double
  , defaultYMin       :: Double
  , defaultYMax       :: Double
  , defaultFont       :: String -- TODO Font("SansSerif", Font.PLAIN, 16)
  }

instance Default DrawConfig where
  def =
    DrawConfig
      { defaultPenColor = black
      , defaultClearColor = white
      , defaultSize = 512
      , defaultPenRadius = 0.002
      , border = 0.0
      , defaultXMin = 0.0
      , defaultXMax = 1.0
      , defaultYMin = 0.0
      , defaultYMax = 1.0
      , defaultFont = "Sans..."
      }

data DrawState = DrawState
  { width     :: Int
  , height    :: Int
  , penColor  :: Color
  , penRadius :: Double
  , defer     :: Bool -- show we draw immediately or wait until next show?
  , xmin      :: Double
  , ymin      :: Double
  , xmax      :: Double
  , ymax      :: Double
  , font      :: String
  , keysTyped :: TChan Char
  , window    :: GLFW.Window
  }

makeState :: DrawConfig -> TChan Char -> GLFW.Window -> DrawState
makeState DrawConfig {..} keysTypedChan w =
  DrawState
    { width = defaultSize
    , height = defaultSize
    , penColor = defaultPenColor
    , penRadius = defaultPenRadius
    , defer = False
    , xmin = defaultXMin
    , ymin = defaultYMin
    , xmax = defaultXMax
    , ymax = defaultYMax
    , font = defaultFont
    , keysTyped = keysTypedChan
    , window = w
    }

newtype DrawApp a = DrawApp
  { runApp :: ReaderT DrawConfig (StateT DrawState IO) a
  } deriving ( Monad
             , MonadIO
             , MonadReader DrawConfig
             , MonadState DrawState
             , Functor
             , Applicative
             )

runDrawApp :: DrawApp a -> DrawConfig -> TChan Char -> GLFW.Window -> IO (a, DrawState)
runDrawApp app config keysTypedChan window =
  let state = makeState config keysTypedChan window
   in runStateT (runReaderT (runApp app) config) state

withWindow :: String
           -> DrawConfig
           -> DrawApp ()
           -> IO ()
withWindow title conf app = do
  hSetBuffering stdout LineBuffering

  True <- GLFW.init

  let a = defaultSize conf
  Just window <- GLFW.createWindow a a title Nothing Nothing
  GLFW.makeContextCurrent (Just window)

  keysTypedChan <- newTChanIO :: IO (TChan Char)
  GLFW.setCharCallback window $ Just (charCallback keysTypedChan)

  forkIO pollEvents

  ((), state) <- runDrawApp app conf keysTypedChan window
  GLFW.terminate

pollEvents = do
  threadDelay (16 * 1000)
  GLFW.pollEvents
  pollEvents

charCallback :: TChan Char -> GLFW.CharCallback
charCallback chan _ char = atomically $ writeTChan chan char

keyCallback :: TChan KeyEvent -> GLFW.KeyCallback
keyCallback chan _ key _ GLFW.KeyState'Pressed _ = atomically $ writeTChan chan $ KeyDown key
keyCallback chan _ key _ GLFW.KeyState'Released _ = atomically $ writeTChan chan $ KeyUp key
keyCallback _ _ _ _ _ _ = return ()

-- | Sets the canvas (drawing area) to be 512-by-512 pixels.
-- | This also erases the current drawing and resets the coordinate system,
-- | pen radius, pen color, and font back to their default values.
-- | Ordinarly, this method is called once, at the very beginning
-- | of a program.
setDefaultCanvasSize :: DrawApp ()
setDefaultCanvasSize = do
  d <- asks defaultSize
  setCanvasSize d d

-- | Sets the canvas (drawing area) to be 'width'-by-'height' pixels.
-- | This also erases the current drawing and resets the coordinate system,
-- | pen radius, pen color, and font back to their default values.
-- | Ordinarly, this method is called once, at the very beginning
-- | of a program.
setCanvasSize :: Int -- ^ canvasWidth the width as a number of pixels
              -> Int -- ^ canvasHeight the height as a number of pixels
              -> DrawApp ()
setCanvasSize w h = do
  when (w <= 0 || h <= 0) $ error "width and height must be positive"
  modify (\s -> s {width = w, height = h})
  initState

  setDefaultScale

  setDefaultPenColor
  setDefaultPenRadius
  setDefaultFont

  clearDefault
-- // add antialiasing
-- RenderingHints hints = new RenderingHints(RenderingHints.KEY_ANTIALIASING,
--                                           RenderingHints.VALUE_ANTIALIAS_ON);
-- hints.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
-- offscreen.addRenderingHints(hints);
-- frame.setResizable(false);

initState :: DrawApp ()
initState = do
  makeNewState <- asks makeState
  kt <- gets keysTyped
  w <- gets window
  put $ makeNewState kt w

-- JMenuBar menuBar = new JMenuBar();
-- JMenu menu = new JMenu("File");
-- menuBar.add(menu);
-- JMenuItem menuItem1 = new JMenuItem(" Save...   ");
-- menuItem1.addActionListener(std);
-- menuItem1.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S,
--                         Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
-- menu.add(menuItem1);
-- return menuBar;

createMenuBar :: DrawApp ()
createMenuBar = undefined

-- | Sets the 'x'-scale to be the default (between 0.0 and 1.0).
setDefaultXscale :: DrawApp ()
setDefaultXscale = do
  xmin <- asks defaultXMin
  xmax <- asks defaultXMax
  setXscale xmin xmax

-- | Sets the 'y'-scale to be the default (between 0.0 and 1.0).
setDefaultYscale :: DrawApp ()
setDefaultYscale = do
  ymin <- asks defaultYMin
  ymax <- asks defaultYMax
  setYscale ymin ymax

-- | Sets the 'x'-scale and 'y'-scale to be the default (between 0.0 and 1.0).
setDefaultScale :: DrawApp ()
setDefaultScale = do
  setDefaultXscale
  setDefaultYscale

-- | Sets the 'x'-scale to the specified range.
setXscale :: Double -- ^ The minimum value of the 'x'-scale
          -> Double -- ^ The maximum value of the 'x'-scale
          -> DrawApp ()
setXscale min max = do
  let size = max - min
  when (size == 0) $ error "the min and max are the same"
  b <- asks border
  modify (\s -> s {xmin = min - b * size, xmax = max + b * size})
  updateScale

-- | Sets the 'y'-scale to the specified range.
setYscale :: Double -- ^ The minimum value of the 'y'-scale
          -> Double -- ^ The maximum value of the 'y'-scale
          -> DrawApp ()
setYscale min max = do
  let size = max - min
  when (size == 0) $ error "the min and max are the same"
  b <- asks border
  modify (\s -> s {ymin = min - b * size, ymax = max + b * size})
  updateScale

-- | Sets both the 'x'-scale and the 'y'-scale to the (same) specified range.
setScale :: Double -- ^ The minimum value of the 'x'- and 'y'-scales
         -> Double -- ^ The maximum value of the 'x'- and 'y'-scales
         -> DrawApp ()
setScale min max = do
  setXscale min max
  setYscale min max

-- | Private helper
updateScale :: DrawApp ()
updateScale = do
  DrawState{xmin, xmax, ymin, ymax} <- get
  liftIO $ do
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho xmin xmax ymin ymax 0 1 -- left, right, top, bottom, nearVal, farVal

-- | Clears the screen to the default color (white).
clearDefault :: DrawApp ()
clearDefault = do
  c <- asks defaultClearColor
  clear c

-- | Clears the screen to the specified color.
clear :: Color -> DrawApp ()
clear (GL.Color3 r g b) = do
  GL.clearColor $= GL.Color4 ((fromIntegral r)/255.0) ((fromIntegral g)/255.0) ((fromIntegral b)/255.0) 1.0
  liftIO $ GL.clear [GL.ColorBuffer]

-- | Returns the current pen radius.
getPenRadius :: DrawApp Double
getPenRadius = do
--   r <- GL.get GL.lineWidth TODO scale this value back?
  r <- gets penRadius
  return r

-- | Sets the pen size to the default size (0.002).
-- | The pen is circular, so that lines have rounded ends, and when you set the
-- | pen radius and draw a point, you get a circle of the specified radius.
-- | The pen radius is not affected by coordinate scaling.
setDefaultPenRadius :: DrawApp ()
setDefaultPenRadius = do
  r <- asks defaultPenRadius
  setPenRadius r

-- | Sets the radius of the pen to the specified size.
-- | The pen is circular, so that lines have rounded ends, and when you set the
-- | pen radius and draw a point, you get a circle of the specified radius.
-- | The pen radius is not affected by coordinate scaling.
setPenRadius :: Double -- ^ the radius of the pen
             -> DrawApp ()
setPenRadius r = do
  when (r < 0) $ error "pen radius must be nonnegative"
  d <- asks defaultPenRadius -- TODO scale by screen size
  GL.lineWidth $= realToFrac (r / d)
  modify (\s -> s {penRadius = r})

-- | Returns the current pen color.
getPenColor :: DrawApp Color
getPenColor = do
  c <- gets penColor
  return c

-- | Set the pen color to the default color (black).
setDefaultPenColor :: DrawApp ()
setDefaultPenColor = do
  c <- asks defaultPenColor
  setPenColor c

-- | Sets the pen color to the specified color.
setPenColor :: Color -> DrawApp ()
setPenColor c = do
  liftIO $ GL.color c
  modify (\s -> s {penColor = c})

-- | Sets the pen color to the specified RGB color.
setRGBPenColor :: GL.GLubyte -- ^ the amount of red (between 0 and 255)
               -> GL.GLubyte -- ^ the amount of green (between 0 and 255)
               -> GL.GLubyte -- ^ the amount of blue (between 0 and 255)
               -> DrawApp ()
setRGBPenColor r g b = do
  setPenColor $ GL.Color3 r g b

-- | Returns the current font.
getFont :: DrawApp String
getFont = do
  f <- gets font
  return f

-- | Sets the font to the default font (sans serif, 16 point).
setDefaultFont :: DrawApp ()
setDefaultFont = do
  f <- asks defaultFont
  setFont f

-- | Sets the font to the specified value.
setFont :: String -> DrawApp ()
setFont f = do
  modify (\s -> s {font = f})

-- | Draws a line segment between ('x0', 'y0') and ('x1', 'y1').
line :: Double -- ^ x0 the 'x'-coordinate of one endpoint
     -> Double -- ^ y0 the 'y'-coordinate of one endpoint
     -> Double -- ^ x1 the 'x'-coordinate of the other endpoint
     -> Double -- ^ y1 the 'y'-coordinate of the other endpoint
     -> DrawApp ()
line x0 y0 x1 y1 = do
  liftIO $ GL.renderPrimitive GL.Lines $ do
    GL.vertex $ vertex3 (x0, y0)
    GL.vertex $ vertex3 (x1, y1)

-- | Draws a connected line
lineStrip :: [(Double, Double)] -- Each pair represents an ('x', 'y') coordinate
          -> DrawApp ()
lineStrip xys = do
  let v3s = fmap vertex3 xys
  liftIO $ GL.renderPrimitive GL.LineStrip $ mapM_ GL.vertex v3s

-- | Draws one pixel at ('x', 'y').
-- | This method is private because pixels depend on the display.
-- | To achieve the same effect, set the pen radius to 0 and call 'point'.
--         offscreen.fillRect((int) Math.round(scaleX(x)), (int) Math.round(scaleY(y)), 1, 1);
pixel :: Double -- ^ the 'x'-coordinate of the pixel
      -> Double -- ^ the 'y'-coordinate of the pixel
      -> DrawApp()
pixel x y = undefined

-- | Draws a point centered at ('x', 'y').
-- | The point is a filled circle whose radius is equal to the pen radius.
-- | To draw a single-pixel point, first set the pen radius to 0.
point :: Double -- ^the 'x'-coordinate of the point
      -> Double -- ^the 'y'-coordinate of the point
      -> DrawApp()
point x y = do
  r <- gets penRadius
  filledCircle x y r

-- | Draws a circle of the specified radius, centered at ('x', 'y').
circle :: Double -- ^ the 'x'-coordinate of the center of the circle
       -> Double -- ^ the 'y'-coordinate of the center of the circle
       -> Double -- ^ radius the radius of the circle
       -> DrawApp ()
circle x y r = ellipse x y r r

-- | Draws a filled circle of the specified radius, centered at ('x', 'y').
filledCircle :: Double -- ^ the 'x'-coordinate of the center of the circle
             -> Double -- ^ the 'y'-coordinate of the center of the circle
             -> Double -- ^ radius the radius of the circle
             -> DrawApp ()
filledCircle x y r = filledEllipse x y r r

-- | Draws an ellipse with the specified semimajor and semiminor axes,
-- | centered at ('x', 'y').
ellipse :: Double -- ^ the 'x'-coordinate of the center of the ellipse
        -> Double -- ^ the 'y'-coordinate of the center of the ellipse
        -> Double -- ^ semiMajorAxis is the semimajor axis of the ellipse
        -> Double -- ^ semiMinorAxis is the semiminor axis of the ellipse
        -> DrawApp ()
ellipse x y semiMajorAxis semiMinorAxis =
  let (xs, ys) = unzip (ellipseVertices x y semiMajorAxis semiMinorAxis)
   in polygon xs ys

-- | Draws an ellipse with the specified semimajor and semiminor axes,
-- | centered at ('x', 'y').
filledEllipse :: Double -- ^ the 'x'-coordinate of the center of the ellipse
        -> Double -- ^ the 'y'-coordinate of the center of the ellipse
        -> Double -- ^ semiMajorAxis is the semimajor axis of the ellipse
        -> Double -- ^ semiMinorAxis is the semiminor axis of the ellipse
        -> DrawApp ()
filledEllipse x y semiMajorAxis semiMinorAxis =
  let (xs, ys) = unzip (ellipseVertices x y semiMajorAxis semiMinorAxis)
   in filledPolygon xs ys

-- | Draws a circular arc of the specified radius,
-- | centered at ('x', 'y'), from angle1 to angle2 (in degrees).
arc :: Double -- ^ the 'x'-coordinate of the center of the circle
    -> Double -- ^ the 'y'-coordinate of the center of the circle
    -> Double -- ^ radius the radius of the circle
    -> Double -- ^ angle1 the starting angle. 0 would mean an arc beginning at 3 o'clock.
    -> Double -- ^ angle2 the angle at the end of the arc. For example, if you want a 90 degree arc, then angle2 should be angle1 + 90.
    -> DrawApp ()
arc x y r angle1 angle2 = do
  let a1 = mod2pi $ degToRad angle1
      a2 = mod2pi $ degToRad angle2
      d  = mod2pi $ a2 - a1
      n  = round $ 300 * d / (2 * pi)
      v  = fmap circleVertex $ fmap (\x -> a1 + d / (fromIntegral n) * fromIntegral x) [0..(n - 1)]
  lineStrip v
  where
    circleVertex angle = (x + r * (cos angle), y + r * (sin angle))

-- | Draws a square of side length 2r, centered at ('x', 'y').
square :: Double -- ^ the 'x'-coordinate of the center of the square
       -> Double -- ^ the 'y'-coordinate of the center of the square
       -> Double -- ^ halfLength one half the length of any side of the square
       -> DrawApp ()
square x y halfLength = rectangle x y halfLength halfLength

-- | Draws a filled square of the specified size, centered at ('x', 'y').
filledSquare :: Double -- ^ the 'x'-coordinate of the center of the square
             -> Double -- ^ the 'y'-coordinate of the center of the square
             -> Double -- ^ halfLength one half the length of any side of the square
             -> DrawApp ()
filledSquare x y halfLength = filledRectangle x y halfLength halfLength

-- | Draws a rectangle of the specified size, centered at ('x', 'y').
rectangle :: Double -- ^ the 'x'-coordinate of the center of the rectangle
          -> Double -- ^ the 'y'-coordinate of the center of the rectangle
          -> Double -- ^ halfWidth one half the width of the rectangle
          -> Double -- ^ halfHeight one half the height of the rectangle
          -> DrawApp ()
rectangle x y hw hh =
  -- TODO draw pixel if very small...
  polygon [x - hw, x - hw, x + hw, x + hw] [y + hh, y - hh, y - hh, y + hh]

-- | Draws a filled rectangle of the specified size, centered at ('x', 'y').
filledRectangle :: Double -- ^ the 'x'-coordinate of the center of the rectangle
                -> Double -- ^ the 'y'-coordinate of the center of the rectangle
                -> Double -- ^ halfWidth one half the width of the rectangle
                -> Double -- ^ halfHeight one half the height of the rectangle
                -> DrawApp ()
filledRectangle x y hw hh =
  -- TODO draw pixel if very small...
  filledPolygon [x - hw, x - hw, x + hw, x + hw] [y + hh, y - hh, y - hh, y + hh]

-- | Draws a polygon with the vertices
-- |   ('x0', 'y0),
-- |   ('x1', 'y1), ...,
-- |   ('xn', 'yn).
polygon :: [Double] -- ^ x an array of all the 'x'-coordinates of the polygon
        -> [Double] -- ^ y an array of all the 'y'-coordinates of the polygon
        -> DrawApp ()
polygon xs ys = do
  liftIO $ GL.renderPrimitive GL.LineLoop $ mapM_ GL.vertex $ verticesUnsafe xs ys

-- | Draws a polygon with the vertices
-- |   ('x0', 'y0),
-- |   ('x1', 'y1), ...,
-- |   ('xn', 'yn).
filledPolygon :: [Double] -- ^ x an array of all the 'x'-coordinates of the polygon
              -> [Double] -- ^ y an array of all the 'y'-coordinates of the polygon
              -> DrawApp ()
filledPolygon xs ys = do
  liftIO $ GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex $ verticesUnsafe xs ys

--     // get an image from the given filename
--     private static Image getImage(String filename) {
--         if (filename == null) throw new IllegalArgumentException();
--
--         // to read from file
--         ImageIcon icon = new ImageIcon(filename);
--
--         // try to read from URL
--         if ((icon == null) || (icon.getImageLoadStatus() != MediaTracker.COMPLETE)) {
--             try {
--                 URL url = new URL(filename);
--                 icon = new ImageIcon(url);
--             }
--             catch (MalformedURLException e) {
--                 /* not a url */
--             }
--         }
--
--         // in case file is inside a .jar (classpath relative to StdDraw)
--         if ((icon == null) || (icon.getImageLoadStatus() != MediaTracker.COMPLETE)) {
--             URL url = StdDraw.class.getResource(filename);
--             if (url != null)
--                 icon = new ImageIcon(url);
--         }
--
--         // in case file is inside a .jar (classpath relative to root of jar)
--         if ((icon == null) || (icon.getImageLoadStatus() != MediaTracker.COMPLETE)) {
--             URL url = StdDraw.class.getResource("/" + filename);
--             if (url == null) throw new IllegalArgumentException("image " + filename + " not found");
--             icon = new ImageIcon(url);
--         }
--
--         return icon.getImage();
--     }
--     /**
--      * Draws the specified image centered at ('x', 'y').
--      * The supported image formats are JPEG, PNG, and GIF.
--      * As an optimization, the picture is cached, so there is no performance
--      * penalty for redrawing the same image multiple times (e.g., in an animation).
--      * However, if you change the picture file after drawing it, subsequent
--      * calls will draw the original picture.
--      *
--      * @param  x the center 'x'-coordinate of the image
--      * @param  y the center 'y'-coordinate of the image
--      * @param  filename the name of the image/picture, e.g., "ball.gif"
--      * @throws IllegalArgumentException if the image filename is invalid
--      */
--     public static void picture(double x, double y, String filename) {
--         // BufferedImage image = getImage(filename);
--         Image image = getImage(filename);
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         // int ws = image.getWidth();    // can call only if image is a BufferedImage
--         // int hs = image.getHeight();
--         int ws = image.getWidth(null);
--         int hs = image.getHeight(null);
--         if (ws < 0 || hs < 0) throw new IllegalArgumentException("image " + filename + " is corrupt");
--
--         offscreen.drawImage(image, (int) Math.round(xs - ws/2.0), (int) Math.round(ys - hs/2.0), null);
--         draw();
--     }
--     /**
--      * Draws the specified image centered at ('x', 'y'),
--      * rotated given number of degrees.
--      * The supported image formats are JPEG, PNG, and GIF.
--      *
--      * @param  x the center 'x'-coordinate of the image
--      * @param  y the center 'y'-coordinate of the image
--      * @param  filename the name of the image/picture, e.g., "ball.gif"
--      * @param  degrees is the number of degrees to rotate counterclockwise
--      * @throws IllegalArgumentException if the image filename is invalid
--      */
--     public static void picture(double x, double y, String filename, double degrees) {
--         // BufferedImage image = getImage(filename);
--         Image image = getImage(filename);
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         // int ws = image.getWidth();    // can call only if image is a BufferedImage
--         // int hs = image.getHeight();
--         int ws = image.getWidth(null);
--         int hs = image.getHeight(null);
--         if (ws < 0 || hs < 0) throw new IllegalArgumentException("image " + filename + " is corrupt");
--         offscreen.rotate(Math.toRadians(-degrees), xs, ys);
--         offscreen.drawImage(image, (int) Math.round(xs - ws/2.0), (int) Math.round(ys - hs/2.0), null);
--         offscreen.rotate(Math.toRadians(+degrees), xs, ys);
--         draw();
--     }
--     /**
--      * Draws the specified image centered at ('x', 'y'),
--      * rescaled to the specified bounding box.
--      * The supported image formats are JPEG, PNG, and GIF.
--      *
--      * @param  x the center 'x'-coordinate of the image
--      * @param  y the center 'y'-coordinate of the image
--      * @param  filename the name of the image/picture, e.g., "ball.gif"
--      * @param  scaledWidth the width of the scaled image (in screen coordinates)
--      * @param  scaledHeight the height of the scaled image (in screen coordinates)
--      * @throws IllegalArgumentException if either {@code scaledWidth}
--      *         or {@code scaledHeight} is negative
--      * @throws IllegalArgumentException if the image filename is invalid
--      */
--     public static void picture(double x, double y, String filename, double scaledWidth, double scaledHeight) {
--         Image image = getImage(filename);
--         if (scaledWidth  < 0) throw new IllegalArgumentException("width  is negative: " + scaledWidth);
--         if (scaledHeight < 0) throw new IllegalArgumentException("height is negative: " + scaledHeight);
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         double ws = factorX(scaledWidth);
--         double hs = factorY(scaledHeight);
--         if (ws < 0 || hs < 0) throw new IllegalArgumentException("image " + filename + " is corrupt");
--         if (ws <= 1 && hs <= 1) pixel(x, y);
--         else {
--             offscreen.drawImage(image, (int) Math.round(xs - ws/2.0),
--                                        (int) Math.round(ys - hs/2.0),
--                                        (int) Math.round(ws),
--                                        (int) Math.round(hs), null);
--         }
--         draw();
--     }
--     /**
--      * Draws the specified image centered at ('x', 'y'), rotated
--      * given number of degrees, and rescaled to the specified bounding box.
--      * The supported image formats are JPEG, PNG, and GIF.
--      *
--      * @param  x the center 'x'-coordinate of the image
--      * @param  y the center 'y'-coordinate of the image
--      * @param  filename the name of the image/picture, e.g., "ball.gif"
--      * @param  scaledWidth the width of the scaled image (in screen coordinates)
--      * @param  scaledHeight the height of the scaled image (in screen coordinates)
--      * @param  degrees is the number of degrees to rotate counterclockwise
--      * @throws IllegalArgumentException if either {@code scaledWidth}
--      *         or {@code scaledHeight} is negative
--      * @throws IllegalArgumentException if the image filename is invalid
--      */
--     public static void picture(double x, double y, String filename, double scaledWidth, double scaledHeight, double degrees) {
--         if (scaledWidth < 0) throw new IllegalArgumentException("width is negative: " + scaledWidth);
--         if (scaledHeight < 0) throw new IllegalArgumentException("height is negative: " + scaledHeight);
--         Image image = getImage(filename);
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         double ws = factorX(scaledWidth);
--         double hs = factorY(scaledHeight);
--         if (ws < 0 || hs < 0) throw new IllegalArgumentException("image " + filename + " is corrupt");
--         if (ws <= 1 && hs <= 1) pixel(x, y);
--
--         offscreen.rotate(Math.toRadians(-degrees), xs, ys);
--         offscreen.drawImage(image, (int) Math.round(xs - ws/2.0),
--                                    (int) Math.round(ys - hs/2.0),
--                                    (int) Math.round(ws),
--                                    (int) Math.round(hs), null);
--         offscreen.rotate(Math.toRadians(+degrees), xs, ys);
--
--         draw();
--     }
--     /**
--      * Write the given text string in the current font, centered at ('x', 'y').
--      *
--      * @param  x the center 'x'-coordinate of the text
--      * @param  y the center 'y'-coordinate of the text
--      * @param  text the text to write
--      */
--     public static void text(double x, double y, String text) {
--         if (text == null) throw new IllegalArgumentException();
--         offscreen.setFont(font);
--         FontMetrics metrics = offscreen.getFontMetrics();
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         int ws = metrics.stringWidth(text);
--         int hs = metrics.getDescent();
--         offscreen.drawString(text, (float) (xs - ws/2.0), (float) (ys + hs));
--         draw();
--     }
--     /**
--      * Write the given text string in the current font, centered at ('x', 'y') and
--      * rotated by the specified number of degrees.
--      * @param  x the center 'x'-coordinate of the text
--      * @param  y the center 'y'-coordinate of the text
--      * @param  text the text to write
--      * @param  degrees is the number of degrees to rotate counterclockwise
--      */
--     public static void text(double x, double y, String text, double degrees) {
--         if (text == null) throw new IllegalArgumentException();
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         offscreen.rotate(Math.toRadians(-degrees), xs, ys);
--         text(x, y, text);
--         offscreen.rotate(Math.toRadians(+degrees), xs, ys);
--     }
--     /**
--      * Write the given text string in the current font, left-aligned at ('x', 'y').
--      * @param  x the 'x'-coordinate of the text
--      * @param  y the 'y'-coordinate of the text
--      * @param  text the text
--      */
--     public static void textLeft(double x, double y, String text) {
--         if (text == null) throw new IllegalArgumentException();
--         offscreen.setFont(font);
--         FontMetrics metrics = offscreen.getFontMetrics();
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         int hs = metrics.getDescent();
--         offscreen.drawString(text, (float) xs, (float) (ys + hs));
--         draw();
--     }
--     /**
--      * Write the given text string in the current font, right-aligned at ('x', 'y').
--      *
--      * @param  x the 'x'-coordinate of the text
--      * @param  y the 'y'-coordinate of the text
--      * @param  text the text to write
--      */
--     public static void textRight(double x, double y, String text) {
--         if (text == null) throw new IllegalArgumentException();
--         offscreen.setFont(font);
--         FontMetrics metrics = offscreen.getFontMetrics();
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         int ws = metrics.stringWidth(text);
--         int hs = metrics.getDescent();
--         offscreen.drawString(text, (float) (xs - ws), (float) (ys + hs));
--         draw();
--     }


-- | Pause for t milliseconds. This method is intended to support computer animations.
pause :: Int -- ^ number of milliseconds
      -> DrawApp ()
pause t = do
  liftIO $ threadDelay (t * 1000)

-- | Copies offscreen buffer to onscreen buffer. There is no reason to call
-- | this method unless double buffering is enabled.
showBuffer :: DrawApp ()
showBuffer = do
  w <- gets window
  liftIO $ GLFW.swapBuffers w


privateShow :: DrawApp ()
privateShow = do
  d <- gets defer
  when (not d) showBuffer

-- | Enable double buffering. All subsequent calls to
-- | drawing methods such as {@code line()}, {@code circle()},
-- | and {@code square()} will be deffered until the next call
-- | to show(). Useful for animations.
enableDoubleBuffering :: DrawApp ()
enableDoubleBuffering = do
  modify (\s -> s { defer = True })

-- | Disable double buffering. All subsequent calls to
-- | drawing methods such as {@code line()}, {@code circle()},
-- | and {@code square()} will be displayed on screen when called.
-- | This is the default.
disableDoubleBuffering :: DrawApp ()
disableDoubleBuffering = do
  modify (\s -> s { defer = False })

-- | Saves the drawing to using the specified filename.
-- | The supported image formats are JPEG and PNG;
-- | the filename suffix must be {@code .jpg} or {@code .png}.
--      *
--      * @param  filename the name of the file with one of the required suffixes
--      */
--     public static void save(String filename) {
--         if (filename == null) throw new IllegalArgumentException();
--         File file = new File(filename);
--         String suffix = filename.substring(filename.lastIndexOf('.') + 1);
--
--         // png files
--         if ("png".equalsIgnoreCase(suffix)) {
--             try {
--                 ImageIO.write(onscreenImage, suffix, file);
--             }
--             catch (IOException e) {
--                 e.printStackTrace();
--             }
--         }
--
--         // need to change from ARGB to RGB for JPEG
--         // reference: http://archives.java.sun.com/cgi-bin/wa?A2=ind0404&L=java2d-interest&D=0&P=2727
--         else if ("jpg".equalsIgnoreCase(suffix)) {
--             WritableRaster raster = onscreenImage.getRaster();
--             WritableRaster newRaster;
--             newRaster = raster.createWritableChild(0, 0, width, height, 0, 0, new int[] {0, 1, 2});
--             DirectColorModel cm = (DirectColorModel) onscreenImage.getColorModel();
--             DirectColorModel newCM = new DirectColorModel(cm.getPixelSize(),
--                                                           cm.getRedMask(),
--                                                           cm.getGreenMask(),
--                                                           cm.getBlueMask());
--             BufferedImage rgbBuffer = new BufferedImage(newCM, newRaster, false,  null);
--             try {
--                 ImageIO.write(rgbBuffer, suffix, file);
--             }
--             catch (IOException e) {
--                 e.printStackTrace();
--             }
--         }
--
--         else {
--             System.out.println("Invalid image file type: " + suffix);
--         }
--     }
--
--
--     /**
--      * This method cannot be called directly.
--      */
--     @Override
--     public void actionPerformed(ActionEvent e) {
--         FileDialog chooser = new FileDialog(StdDraw.frame, "Use a .png or .jpg extension", FileDialog.SAVE);
--         chooser.setVisible(true);
--         String filename = chooser.getFile();
--         if (filename != null) {
--             StdDraw.save(chooser.getDirectory() + File.separator + chooser.getFile());
--         }
--     }
--

-- | Returns true if the mouse is being pressed.
isMousePressed :: DrawApp Bool
isMousePressed = do
  w <- gets window
  state <- liftIO $ GLFW.getMouseButton w GLFW.MouseButton'1
  return $ state == GLFW.MouseButtonState'Pressed

-- | Returns the 'x'-coordinate of the mouse.
mouseX :: DrawApp Double
mouseX = fmap fst mouse

-- | Returns the 'y'-coordinate of the mouse.
mouseY :: DrawApp Double
mouseY = fmap snd mouse

mouse :: DrawApp (Double, Double)
mouse = do
  DrawState {window, xmin, xmax, ymin, ymax, width, height} <- get
  (x, y) <- liftIO $ GLFW.getCursorPos window
  return (xmin + x * (xmax - xmin) / (fromIntegral width), ymax - y * (ymax - ymin) / (fromIntegral height))

--     /**
--      * This method cannot be called directly.
--      */
--     @Override
--     public void mousePressed(MouseEvent e) {
--         synchronized (mouseLock) {
--             mouseX = StdDraw.userX(e.getX());
--             mouseY = StdDraw.userY(e.getY());
--             isMousePressed = true;
--         }
--     }
--
--     /**
--      * This method cannot be called directly.
--      */
--     @Override
--     public void mouseReleased(MouseEvent e) {
--         synchronized (mouseLock) {
--             isMousePressed = false;
--         }
--     }
--
--     /**
--      * This method cannot be called directly.
--      */
--     @Override
--     public void mouseDragged(MouseEvent e)  {
--         synchronized (mouseLock) {
--             mouseX = StdDraw.userX(e.getX());
--             mouseY = StdDraw.userY(e.getY());
--         }
--     }
--
--     /**
--      * This method cannot be called directly.
--      */
--     @Override
--     public void mouseMoved(MouseEvent e) {
--         synchronized (mouseLock) {
--             mouseX = StdDraw.userX(e.getX());
--             mouseY = StdDraw.userY(e.getY());
--         }
--     }
--

-- | Returns true if the user has typed a key (that has not yet been processed).
hasNextKeyTyped :: DrawApp Bool
hasNextKeyTyped = do
  chan <- gets keysTyped
  e <- liftIO $ atomically $ isEmptyTChan chan
  return $ not e

-- | Returns the next key that was typed by the user (that your program has not already processed).
-- | Unlike the Java version this Method waits for the next key instead of throwing an exception.
-- | It cannot identify action keys (such as F1 and arrow keys) or modifier keys (such as control).
nextKeyTyped :: DrawApp Char
nextKeyTyped = do
  chan <- gets keysTyped
  key <- liftIO $ atomically $ readTChan chan
  return key

-- | Returns true if the given key is being pressed.
-- | This method takes the keycode (corresponding to a physical key)
-- |  as an argument. It can handle action keys
-- | (such as F1 and arrow keys) and modifier keys (such as shift and control).
isKeyPressed :: GLFW.Key -- ^ the key to check if it is being pressed
             -> DrawApp Bool
isKeyPressed k = do
  w <- gets window
  state <- liftIO $ GLFW.getKey w k
  return $ state /= GLFW.KeyState'Released

