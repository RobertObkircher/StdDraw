{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE NamedFieldPuns             #-}

module StdDraw
  ( runDrawApp
  , DrawApp
  , DrawState
  , DrawConfig
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, asks, runReaderT,
                                         when)
import           Control.Monad.State    (MonadState, StateT, modify, put,
                                         runStateT, gets, get)
import           Data.Default
import           Graphics.UI.GLUT       (Color3 (..), GLubyte)

type Color = Color3 GLubyte

data DrawConfig = DrawConfig
  { black             :: Color
  , blue              :: Color
  , cyan              :: Color
  , darkGray          :: Color
  , gray              :: Color
  , green             :: Color
  , lightGray         :: Color
  , magenta           :: Color
  , orange            :: Color
  , pink              :: Color
  , red               :: Color
  , white             :: Color
  , yellow            :: Color
  , bookBlue          :: Color
  , bookLightBlue     :: Color
  , bookRed           :: Color
  , princetonOrange   :: Color
  , defaultPenColor   :: Color
  , defaultClearColor :: Color
  , defaultSize       :: Integer
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
      { black = Color3 0 0 0
      , blue = Color3 0 0 255
      , cyan = Color3 0 255 255
      , darkGray = Color3 64 64 64
      , gray = Color3 128 128 128
      , green = Color3 0 255 0
      , lightGray = Color3 192 192 192
      , magenta = Color3 255 0 255
      , orange = Color3 255 200 0
      , pink = Color3 255 175 175
      , red = Color3 255 0 0
      , white = Color3 255 255 255
      , yellow = Color3 255 255 0
      , bookBlue = Color3 9 90 166
      , bookLightBlue = Color3 103 198 243
      , bookRed = Color3 150 35 31
      , princetonOrange = Color3 245 128 37
      , defaultPenColor = Color3 0 0 0 -- TODO = black?
      , defaultClearColor = Color3 255 255 255 -- TODO = white?
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
  { width          :: Integer
  , height         :: Integer
  , penColor       :: Color
  , penRadius      :: Double
  , defer          :: Bool -- show we draw immediately or wait until next show?
  , xmin           :: Double
  , ymin           :: Double
  , xmax           :: Double
  , ymax           :: Double
  , font           :: String
  , mousePressed :: Bool
  , mouseX         :: Double
  , mouseY         :: Double
  , keysTyped      :: [Char] -- ^ Opposite direction than Java, queue of typed key characters
  , keysDown       :: [Integer] -- set of key codes currently pressed down
  }

makeState :: DrawConfig -> DrawState
makeState DrawConfig {..} =
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
    , mousePressed = False
    , mouseX = 0
    , mouseY = 0
    , keysTyped = []
    , keysDown = []
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

runDrawApp :: DrawApp a -> DrawConfig -> IO (a, DrawState)
runDrawApp app config =
  let state = makeState config
   in runStateT (runReaderT (runApp app) config) state

-- | Sets the canvas (drawing area) to be 512-by-512 pixels.
-- | This also erases the current drawing and resets the coordinate system,
-- | pen radius, pen color, and font back to their default values.
-- | Ordinarly, this method is called once, at the very beginning
-- | of a program.
setDefaultCanvasSize :: DrawApp ()
setDefaultCanvasSize = do
  d <- asks defaultSize
  setCanvasSize d d

-- | Sets the canvas (drawing area) to be <em>width</em>-by-<em>height</em> pixels.
-- | This also erases the current drawing and resets the coordinate system,
-- | pen radius, pen color, and font back to their default values.
-- | Ordinarly, this method is called once, at the very beginning
-- | of a program.
-- |
-- | @param  canvasWidth the width as a number of pixels
-- | @param  canvasHeight the height as a number of pixels
-- | @throws IllegalArgumentException unless both {@code canvasWidth} and
-- |         {@code canvasHeight} are positive
-- |
setCanvasSize :: Integer -> Integer -> DrawApp ()
setCanvasSize w h = do
  when (w <= 0 || h <= 0) $ error "width and height must be positive"
  modify (\s -> s {width = w, height = h})
  initState

-- TODO
-- if (frame != null) frame.setVisible(false);
-- frame = new JFrame();
-- offscreenImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
-- onscreenImage  = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
-- offscreen = offscreenImage.createGraphics();
-- onscreen  = onscreenImage.createGraphics();
-- setXscale();
-- setYscale();
-- offscreen.setColor(DEFAULT_CLEAR_COLOR);
-- offscreen.fillRect(0, 0, width, height);
-- setPenColor();
-- setPenRadius();
-- setFont();
-- clear();
--
-- // add antialiasing
-- RenderingHints hints = new RenderingHints(RenderingHints.KEY_ANTIALIASING,
--                                           RenderingHints.VALUE_ANTIALIAS_ON);
-- hints.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
-- offscreen.addRenderingHints(hints);
--
-- // frame stuff
-- ImageIcon icon = new ImageIcon(onscreenImage);
-- JLabel draw = new JLabel(icon);
--
-- draw.addMouseListener(std);
-- draw.addMouseMotionListener(std);
--
-- frame.setContentPane(draw);
-- frame.addKeyListener(std);    // JLabel cannot get keyboard focus
-- frame.setResizable(false);
-- frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);            // closes all windows
-- // frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);      // closes only current window
-- frame.setTitle("Standard Draw");
-- frame.setJMenuBar(createMenuBar());
-- frame.pack();
-- frame.requestFocusInWindow();
-- frame.setVisible(true);
initState :: DrawApp ()
initState = do
  newState <- asks makeState
  put newState

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

-- | Sets the <em>y</em>-scale to be the default (between 0.0 and 1.0).
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

-- | Sets the 'y'-scale to the specified range.
setYscale :: Double -- ^ The minimum value of the 'y'-scale
          -> Double -- ^ The maximum value of the 'y'-scale
          -> DrawApp ()
setYscale min max = do
  let size = max - min
  when (size == 0) $ error "the min and max are the same"
  b <- asks border
  modify (\s -> s {ymin = min - b * size, ymax = max + b * size})

-- | Sets both the 'x'-scale and the 'y'-scale to the (same) specified range.
setScale :: Double -- ^ The minimum value of the 'x'- and 'y'-scales
          -> Double -- ^ The maximum value of the 'x'- and 'y'-scales
          -> DrawApp ()
setScale min max = do
  setXscale min max
  setYscale min max


-- | Helper function that scales from user coordinates to screen coordinates and back
scaleX :: Double -> DrawApp Double
scaleX x = do
  DrawState{width, xmin, xmax} <- get
  return $ (fromInteger width) * (x - xmin) / (xmax - xmin)

-- | Helper function that scales from user coordinates to screen coordinates and back
scaleY :: Double -> DrawApp Double
scaleY y = do
  DrawState{height, ymin, ymax} <- get
  return $ (fromInteger height) * (ymax - y) / (ymax - ymin)

-- | Helper function that scales from user coordinates to screen coordinates and back
factorX :: Double -> DrawApp Double
factorX w = do
  DrawState{width, xmin, xmax} <- get
  return $ w * (fromInteger width) / abs (xmax - xmin)

-- | Helper function that scales from user coordinates to screen coordinates and back
factorY :: Double -> DrawApp Double
factorY h = do
  DrawState{height, ymin, ymax} <- get
  return $ h * (fromInteger height) / abs (ymax - ymin)

-- | Helper function that scales from user coordinates to screen coordinates and back
userX :: Double -> DrawApp Double
userX x = do
  DrawState{width, xmin, xmax} <- get
  return $ xmin + x * (xmax - xmin) / (fromInteger width)

-- | Helper function that scales from user coordinates to screen coordinates and back
userY :: Double -> DrawApp Double
userY y = do
  DrawState{height, ymin, ymax} <- get
  return $ ymax - y * (ymax - ymin) / (fromInteger height)

-- | Clears the screen to the default color (white).
clearDefault :: DrawApp ()
clearDefault = do
  c <- asks defaultClearColor
  clear c

-- | Clears the screen to the specified color.
-- |   public static void clear(Color color) {
-- |       offscreen.setColor(color);
-- |       offscreen.fillRect(0, 0, width, height);
-- |       offscreen.setColor(penColor);
-- |       draw();
-- |   }
clear :: Color -> DrawApp ()
clear c = undefined


-- | Returns the current pen radius.
getPenRadius :: DrawApp Double
getPenRadius = do
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
-- |   if (!(radius >= 0)) throw new IllegalArgumentException("pen radius must be nonnegative");
-- |   penRadius = radius;
-- |   float scaledPenRadius = (float) (radius * DEFAULT_SIZE);
-- |   BasicStroke stroke = new BasicStroke(scaledPenRadius, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND);
-- |   // BasicStroke stroke = new BasicStroke(scaledPenRadius);
-- |   offscreen.setStroke(stroke);
setPenRadius :: Double -- ^ the radius of the pen
             -> DrawApp ()
setPenRadius r = do
  when (r < 0) $ error "pen radius must be nonnegative"
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
-- |   offscreen.setColor(penColor);
setPenColor :: Color -> DrawApp ()
setPenColor c = do
  modify (\s -> s {penColor = c})

-- | Sets the pen color to the specified RGB color.
setRGBPenColor :: GLubyte -- ^ the amount of red (between 0 and 255)
            -> GLubyte -- ^ the amount of green (between 0 and 255)
            -> GLubyte -- ^ the amount of blue (between 0 and 255)
            -> DrawApp ()
setRGBPenColor r g b = do
  setPenColor $ Color3 r g b

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
--         offscreen.draw(new Line2D.Double(scaleX(x0), scaleY(y0), scaleX(x1), scaleY(y1)));
--         draw();
line :: Double -- ^ x0 the 'x'-coordinate of one endpoint
     -> Double -- ^ y0 the 'y'-coordinate of one endpoint
     -> Double -- ^ x1 the 'x'-coordinate of the other endpoint
     -> Double -- ^ y1 the 'y'-coordinate of the other endpoint
     -> DrawApp ()
line = undefined

-- | Draws one pixel at ('x', 'y').
-- | This method is private because pixels depend on the display.
-- | To achieve the same effect, set the pen radius to 0 and call 'point'.
--         offscreen.fillRect((int) Math.round(scaleX(x)), (int) Math.round(scaleY(y)), 1, 1);
pixel :: Double -- ^ the 'x'-coordinate of the pixel
      -> Double -- ^ the 'y'-coordinate of the pixel
      -> DrawApp()
pixel x y = undefined

-- | Draws a point centered at (<em>x</em>, <em>y</em>).
-- | The point is a filled circle whose radius is equal to the pen radius.
-- | To draw a single-pixel point, first set the pen radius to 0.
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         double r = penRadius;
--         float scaledPenRadius = (float) (r * DEFAULT_SIZE);
--
--         // double ws = factorX(2*r);
--         // double hs = factorY(2*r);
--         // if (ws <= 1 && hs <= 1) pixel(x, y);
--         if (scaledPenRadius <= 1) pixel(x, y);
--         else offscreen.fill(new Ellipse2D.Double(xs - scaledPenRadius/2, ys - scaledPenRadius/2,
--                                                  scaledPenRadius, scaledPenRadius));
--         draw();
point :: Double -- ^the 'x'-coordinate of the point
      -> Double -- ^the 'y'-coordinate of the point
      -> DrawApp()
point x y = undefined

-- | Draws a circle of the specified radius, centered at ('x', 'y').
--     public static void circle(double x, double y, double radius) {
--         if (!(radius >= 0)) throw new IllegalArgumentException("radius must be nonnegative");
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         double ws = factorX(2*radius);
--         double hs = factorY(2*radius);
--         if (ws <= 1 && hs <= 1) pixel(x, y);
--         else offscreen.draw(new Ellipse2D.Double(xs - ws/2, ys - hs/2, ws, hs));
--         draw();
--     }
circle :: Double -- ^ the 'x'-coordinate of the center of the circle
       -> Double -- ^ the 'y'-coordinate of the center of the circle
       -> Double -- ^ radius the radius of the circle
       -> DrawApp()
circle x y r = undefined

--      * Draws a filled circle of the specified radius, centered at (<em>x</em>, <em>y</em>).
--      * @param  x the <em>x</em>-coordinate of the center of the circle
--      * @param  y the <em>y</em>-coordinate of the center of the circle
--      * @param  radius the radius of the circle
--      * @throws IllegalArgumentException if {@code radius} is negative
--      */
--     public static void filledCircle(double x, double y, double radius) {
--         if (!(radius >= 0)) throw new IllegalArgumentException("radius must be nonnegative");
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         double ws = factorX(2*radius);
--         double hs = factorY(2*radius);
--         if (ws <= 1 && hs <= 1) pixel(x, y);
--         else offscreen.fill(new Ellipse2D.Double(xs - ws/2, ys - hs/2, ws, hs));
--         draw();
--     }
--     /**
--      * Draws an ellipse with the specified semimajor and semiminor axes,
--      * centered at (<em>x</em>, <em>y</em>).
--      *
--      * @param  x the <em>x</em>-coordinate of the center of the ellipse
--      * @param  y the <em>y</em>-coordinate of the center of the ellipse
--      * @param  semiMajorAxis is the semimajor axis of the ellipse
--      * @param  semiMinorAxis is the semiminor axis of the ellipse
--      * @throws IllegalArgumentException if either {@code semiMajorAxis}
--      *         or {@code semiMinorAxis} is negative
--      */
--     public static void ellipse(double x, double y, double semiMajorAxis, double semiMinorAxis) {
--         if (!(semiMajorAxis >= 0)) throw new IllegalArgumentException("ellipse semimajor axis must be nonnegative");
--         if (!(semiMinorAxis >= 0)) throw new IllegalArgumentException("ellipse semiminor axis must be nonnegative");
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         double ws = factorX(2*semiMajorAxis);
--         double hs = factorY(2*semiMinorAxis);
--         if (ws <= 1 && hs <= 1) pixel(x, y);
--         else offscreen.draw(new Ellipse2D.Double(xs - ws/2, ys - hs/2, ws, hs));
--         draw();
--     }
--     /**
--      * Draws an ellipse with the specified semimajor and semiminor axes,
--      * centered at (<em>x</em>, <em>y</em>).
--      *
--      * @param  x the <em>x</em>-coordinate of the center of the ellipse
--      * @param  y the <em>y</em>-coordinate of the center of the ellipse
--      * @param  semiMajorAxis is the semimajor axis of the ellipse
--      * @param  semiMinorAxis is the semiminor axis of the ellipse
--      * @throws IllegalArgumentException if either {@code semiMajorAxis}
--      *         or {@code semiMinorAxis} is negative
--      */
--     public static void filledEllipse(double x, double y, double semiMajorAxis, double semiMinorAxis) {
--         if (!(semiMajorAxis >= 0)) throw new IllegalArgumentException("ellipse semimajor axis must be nonnegative");
--         if (!(semiMinorAxis >= 0)) throw new IllegalArgumentException("ellipse semiminor axis must be nonnegative");
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         double ws = factorX(2*semiMajorAxis);
--         double hs = factorY(2*semiMinorAxis);
--         if (ws <= 1 && hs <= 1) pixel(x, y);
--         else offscreen.fill(new Ellipse2D.Double(xs - ws/2, ys - hs/2, ws, hs));
--         draw();
--     }
--     /**
--      * Draws a circular arc of the specified radius,
--      * centered at (<em>x</em>, <em>y</em>), from angle1 to angle2 (in degrees).
--      *
--      * @param  x the <em>x</em>-coordinate of the center of the circle
--      * @param  y the <em>y</em>-coordinate of the center of the circle
--      * @param  radius the radius of the circle
--      * @param  angle1 the starting angle. 0 would mean an arc beginning at 3 o'clock.
--      * @param  angle2 the angle at the end of the arc. For example, if
--      *         you want a 90 degree arc, then angle2 should be angle1 + 90.
--      * @throws IllegalArgumentException if {@code radius} is negative
--      */
--     public static void arc(double x, double y, double radius, double angle1, double angle2) {
--         if (radius < 0) throw new IllegalArgumentException("arc radius must be nonnegative");
--         while (angle2 < angle1) angle2 += 360;
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         double ws = factorX(2*radius);
--         double hs = factorY(2*radius);
--         if (ws <= 1 && hs <= 1) pixel(x, y);
--         else offscreen.draw(new Arc2D.Double(xs - ws/2, ys - hs/2, ws, hs, angle1, angle2 - angle1, Arc2D.OPEN));
--         draw();
--     }
--     /**
--      * Draws a square of side length 2r, centered at (<em>x</em>, <em>y</em>).
--      *
--      * @param  x the <em>x</em>-coordinate of the center of the square
--      * @param  y the <em>y</em>-coordinate of the center of the square
--      * @param  halfLength one half the length of any side of the square
--      * @throws IllegalArgumentException if {@code halfLength} is negative
--      */
--     public static void square(double x, double y, double halfLength) {
--         if (!(halfLength >= 0)) throw new IllegalArgumentException("half length must be nonnegative");
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         double ws = factorX(2*halfLength);
--         double hs = factorY(2*halfLength);
--         if (ws <= 1 && hs <= 1) pixel(x, y);
--         else offscreen.draw(new Rectangle2D.Double(xs - ws/2, ys - hs/2, ws, hs));
--         draw();
--     }
--     /**
--      * Draws a filled square of the specified size, centered at (<em>x</em>, <em>y</em>).
--      *
--      * @param  x the <em>x</em>-coordinate of the center of the square
--      * @param  y the <em>y</em>-coordinate of the center of the square
--      * @param  halfLength one half the length of any side of the square
--      * @throws IllegalArgumentException if {@code halfLength} is negative
--      */
--     public static void filledSquare(double x, double y, double halfLength) {
--         if (!(halfLength >= 0)) throw new IllegalArgumentException("half length must be nonnegative");
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         double ws = factorX(2*halfLength);
--         double hs = factorY(2*halfLength);
--         if (ws <= 1 && hs <= 1) pixel(x, y);
--         else offscreen.fill(new Rectangle2D.Double(xs - ws/2, ys - hs/2, ws, hs));
--         draw();
--     }
--     /**
--      * Draws a rectangle of the specified size, centered at (<em>x</em>, <em>y</em>).
--      *
--      * @param  x the <em>x</em>-coordinate of the center of the rectangle
--      * @param  y the <em>y</em>-coordinate of the center of the rectangle
--      * @param  halfWidth one half the width of the rectangle
--      * @param  halfHeight one half the height of the rectangle
--      * @throws IllegalArgumentException if either {@code halfWidth} or {@code halfHeight} is negative
--      */
--     public static void rectangle(double x, double y, double halfWidth, double halfHeight) {
--         if (!(halfWidth  >= 0)) throw new IllegalArgumentException("half width must be nonnegative");
--         if (!(halfHeight >= 0)) throw new IllegalArgumentException("half height must be nonnegative");
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         double ws = factorX(2*halfWidth);
--         double hs = factorY(2*halfHeight);
--         if (ws <= 1 && hs <= 1) pixel(x, y);
--         else offscreen.draw(new Rectangle2D.Double(xs - ws/2, ys - hs/2, ws, hs));
--         draw();
--     }
--     /**
--      * Draws a filled rectangle of the specified size, centered at (<em>x</em>, <em>y</em>).
--      *
--      * @param  x the <em>x</em>-coordinate of the center of the rectangle
--      * @param  y the <em>y</em>-coordinate of the center of the rectangle
--      * @param  halfWidth one half the width of the rectangle
--      * @param  halfHeight one half the height of the rectangle
--      * @throws IllegalArgumentException if either {@code halfWidth} or {@code halfHeight} is negative
--      */
--     public static void filledRectangle(double x, double y, double halfWidth, double halfHeight) {
--         if (!(halfWidth  >= 0)) throw new IllegalArgumentException("half width must be nonnegative");
--         if (!(halfHeight >= 0)) throw new IllegalArgumentException("half height must be nonnegative");
--         double xs = scaleX(x);
--         double ys = scaleY(y);
--         double ws = factorX(2*halfWidth);
--         double hs = factorY(2*halfHeight);
--         if (ws <= 1 && hs <= 1) pixel(x, y);
--         else offscreen.fill(new Rectangle2D.Double(xs - ws/2, ys - hs/2, ws, hs));
--         draw();
--     }
--     /**
--      * Draws a polygon with the vertices
--      * (<em>x</em><sub>0</sub>, <em>y</em><sub>0</sub>),
--      * (<em>x</em><sub>1</sub>, <em>y</em><sub>1</sub>), ...,
--      * (<em>x</em><sub><em>n</em>–1</sub>, <em>y</em><sub><em>n</em>–1</sub>).
--      *
--      * @param  x an array of all the <em>x</em>-coordinates of the polygon
--      * @param  y an array of all the <em>y</em>-coordinates of the polygon
--      * @throws IllegalArgumentException unless {@code x[]} and {@code y[]}
--      *         are of the same length
--      */
--     public static void polygon(double[] x, double[] y) {
--         if (x == null) throw new IllegalArgumentException("x-coordinate array is null");
--         if (y == null) throw new IllegalArgumentException("y-coordinate array is null");
--         int n1 = x.length;
--         int n2 = y.length;
--         if (n1 != n2) throw new IllegalArgumentException("arrays must be of the same length");
--         int n = n1;
--         if (n == 0) return;
--
--         GeneralPath path = new GeneralPath();
--         path.moveTo((float) scaleX(x[0]), (float) scaleY(y[0]));
--         for (int i = 0; i < n; i++)
--             path.lineTo((float) scaleX(x[i]), (float) scaleY(y[i]));
--         path.closePath();
--         offscreen.draw(path);
--         draw();
--     }
--     /**
--      * Draws a polygon with the vertices
--      * (<em>x</em><sub>0</sub>, <em>y</em><sub>0</sub>),
--      * (<em>x</em><sub>1</sub>, <em>y</em><sub>1</sub>), ...,
--      * (<em>x</em><sub><em>n</em>–1</sub>, <em>y</em><sub><em>n</em>–1</sub>).
--      *
--      * @param  x an array of all the <em>x</em>-coordinates of the polygon
--      * @param  y an array of all the <em>y</em>-coordinates of the polygon
--      * @throws IllegalArgumentException unless {@code x[]} and {@code y[]}
--      *         are of the same length
--      */
--     public static void filledPolygon(double[] x, double[] y) {
--         if (x == null) throw new IllegalArgumentException("x-coordinate array is null");
--         if (y == null) throw new IllegalArgumentException("y-coordinate array is null");
--         int n1 = x.length;
--         int n2 = y.length;
--         if (n1 != n2) throw new IllegalArgumentException("arrays must be of the same length");
--         int n = n1;
--         if (n == 0) return;
--
--         GeneralPath path = new GeneralPath();
--         path.moveTo((float) scaleX(x[0]), (float) scaleY(y[0]));
--         for (int i = 0; i < n; i++)
--             path.lineTo((float) scaleX(x[i]), (float) scaleY(y[i]));
--         path.closePath();
--         offscreen.fill(path);
--         draw();
--     }
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
--      * Draws the specified image centered at (<em>x</em>, <em>y</em>).
--      * The supported image formats are JPEG, PNG, and GIF.
--      * As an optimization, the picture is cached, so there is no performance
--      * penalty for redrawing the same image multiple times (e.g., in an animation).
--      * However, if you change the picture file after drawing it, subsequent
--      * calls will draw the original picture.
--      *
--      * @param  x the center <em>x</em>-coordinate of the image
--      * @param  y the center <em>y</em>-coordinate of the image
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
--      * Draws the specified image centered at (<em>x</em>, <em>y</em>),
--      * rotated given number of degrees.
--      * The supported image formats are JPEG, PNG, and GIF.
--      *
--      * @param  x the center <em>x</em>-coordinate of the image
--      * @param  y the center <em>y</em>-coordinate of the image
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
--      * Draws the specified image centered at (<em>x</em>, <em>y</em>),
--      * rescaled to the specified bounding box.
--      * The supported image formats are JPEG, PNG, and GIF.
--      *
--      * @param  x the center <em>x</em>-coordinate of the image
--      * @param  y the center <em>y</em>-coordinate of the image
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
--      * Draws the specified image centered at (<em>x</em>, <em>y</em>), rotated
--      * given number of degrees, and rescaled to the specified bounding box.
--      * The supported image formats are JPEG, PNG, and GIF.
--      *
--      * @param  x the center <em>x</em>-coordinate of the image
--      * @param  y the center <em>y</em>-coordinate of the image
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
--      * Write the given text string in the current font, centered at (<em>x</em>, <em>y</em>).
--      *
--      * @param  x the center <em>x</em>-coordinate of the text
--      * @param  y the center <em>y</em>-coordinate of the text
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
--      * Write the given text string in the current font, centered at (<em>x</em>, <em>y</em>) and
--      * rotated by the specified number of degrees.
--      * @param  x the center <em>x</em>-coordinate of the text
--      * @param  y the center <em>y</em>-coordinate of the text
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
--      * Write the given text string in the current font, left-aligned at (<em>x</em>, <em>y</em>).
--      * @param  x the <em>x</em>-coordinate of the text
--      * @param  y the <em>y</em>-coordinate of the text
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
--      * Write the given text string in the current font, right-aligned at (<em>x</em>, <em>y</em>).
--      *
--      * @param  x the <em>x</em>-coordinate of the text
--      * @param  y the <em>y</em>-coordinate of the text
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
pause :: Integer -- ^ number of milliseconds
      -> DrawApp ()
pause t = undefined

-- | Copies offscreen buffer to onscreen buffer. There is no reason to call
-- | this method unless double buffering is enabled.
--         onscreen.drawImage(offscreenImage, 0, 0, null);
--         frame.repaint();
showBuffer :: DrawApp ()
showBuffer = undefined


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

--
--    /***************************************************************************
--     *  Save drawing to a file.
--     ***************************************************************************/
--
--     /**
--      * Saves the drawing to using the specified filename.
--      * The supported image formats are JPEG and PNG;
--      * the filename suffix must be {@code .jpg} or {@code .png}.
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
  p <- gets mousePressed
  return p


-- | Returns the 'x'-coordinate of the mouse.
getMouseX :: DrawApp Double
getMouseX = do
  x <- gets mouseX
  return x

-- | Returns the 'y'-coordinate of the mouse.
getMouseY :: DrawApp Double
getMouseY = do
  y <- gets mouseY
  return y

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
  kt <- gets keysTyped
  return (kt /= [])

-- | Returns the next key that was typed by the user (that your program has not already processed).
-- | This method should be preceded by a call to {@link #hasNextKeyTyped()} to ensure
-- | that there is a next key to process.
-- | This method returns a Unicode character corresponding to the key
-- | typed (such as {@code 'a'} or {@code 'A'}).
-- | It cannot identify action keys (such as F1 and arrow keys)
-- | or modifier keys (such as control).
nextKeyTypedUnsafe :: DrawApp Char
nextKeyTypedUnsafe = do
  (x:xs) <- gets keysTyped
  modify (\s -> s {keysTyped = xs})
  return x

-- | Returns true if the given key is being pressed.
-- | This method takes the keycode (corresponding to a physical key)
-- |  as an argument. It can handle action keys
-- | (such as F1 and arrow keys) and modifier keys (such as shift and control).
isKeyPressed :: Integer -- ^ the key to check if it is being pressed
             -> DrawApp Bool
isKeyPressed keycode = do
  keys <- gets keysDown
  return $ keycode `elem` keys

-- | This method cannot be called directly.
keyTyped :: Char -> DrawApp ()
keyTyped c = do
  modify (\s -> s {keysTyped = (keysTyped s) ++ [c]})


-- | This method cannot be called directly.
keyPressed :: Integer -> DrawApp ()
keyPressed keycode = do
  modify (\s -> s {keysDown = keycode : (keysDown s)})

-- | This method cannot be called directly.
keyReleased :: Integer -> DrawApp ()
keyReleased keycode = do
  modify (\s -> s {keysDown = filter (/= keycode) (keysDown s)})

