
module Attr where


import           Brick.AttrMap
import           Brick.Main
import           Brick.Types
import           Brick.Widgets.Core
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Brick.Widgets.List
import           Brick.Util
import           Graphics.Vty.Input.Events
import           Graphics.Vty                   ( defAttr )
import qualified Graphics.Vty                  as Vty

start :: IO ()
start = do
  initialState <- buildInitialState
  _            <- defaultMain app initialState
  pass

app :: App Int e Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure
  , appAttrMap      = const $ attrMap
    defAttr
    [ (listSelectedAttr, Vty.black `Brick.Util.on` Vty.white)
    , (green           , Brick.Util.fg Vty.green)
    , (red             , Brick.Util.fg Vty.red)
    , (blue            , Brick.Util.fg Vty.blue)
    , (yellow          , Brick.Util.fg Vty.yellow)
    , ( magentaUnderline
      , Vty.withStyle (Brick.Util.fg Vty.magenta) Vty.underline
      )
    , (bold        , Vty.withStyle defAttr Vty.bold)
    , (standout    , Vty.withStyle defAttr Vty.standout)
    , (italic      , Vty.withStyle defAttr Vty.italic)
    , (underline   , Vty.withStyle defAttr Vty.underline)
    , (reverseVideo, Vty.withStyle defAttr Vty.reverseVideo)
    , (dim         , Vty.withStyle defAttr Vty.dim)
    , (blink       , Vty.withStyle defAttr Vty.blink)
    ]
  }

green, red, magentaUnderline, blue, yellow :: AttrName
green = "green"
red = "red"
magentaUnderline = "magentaUnderline"
blue = "blue"
yellow = "yellow"


bold, standout, italic, underline, reverseVideo, dim, blink :: AttrName
bold = "bold"
standout = "standout"
italic = "italic"
underline = "underline"
reverseVideo = "reverseVideo"
dim = "dim"
blink = "blink"


data Name = Queue | Queue0
 deriving (Show, Eq, Ord)




buildInitialState :: IO Int
buildInitialState = do
  pure 3


drawWithAttr :: Widget n
drawWithAttr =
  (str "withAttr\n  magentaUnderline (\n ")
    <=> (withAttr
          magentaUnderline
          (   (withDefAttr green $ center $ str
                "(withDefAttr green \n  $ center \n  $ str \"this\")"
              )
          <=> (withAttr red $ center $ str
                "(withDefAttr red \n  $ center \n  $ str \"this\")"
              )
          <=> (center $ str "(center \n  $ str \"this\")")
          )
        )

drawWithDefAttr :: Widget n
drawWithDefAttr =
  (str "withDefAttr\n  magentaUnderline (\n ")
    <=> (withDefAttr
          magentaUnderline
          (   (withDefAttr green $ center $ str
                "(withDefAttr green \n  $ center \n  $ str \"this\")"
              )
          <=> (withAttr red $ center $ str
                "(withDefAttr red \n  $ center \n  $ str \"this\")"
              )
          <=> (center $ str "(center \n  $ str \"this\")")
          )
        )
overGreen :: Widget n
overGreen =
  (str "overrideAttr\n  green magentaUnderline (\n ")
    <=> (overrideAttr
          green
          magentaUnderline
          (   (withDefAttr green $ center $ str
                "(withDefAttr green \n  $ center \n  $ str \"this\")"
              )
          <=> (withAttr red $ center $ str
                "(withDefAttr red \n  $ center \n  $ str \"this\")"
              )
          <=> (center $ str "(center \n  $ str \"this\")")
          )
        )


overRed :: Widget n
overRed =
  (str "overrideAttr\n  red magentaUnderline (\n ")
    <=> (overrideAttr
          red
          magentaUnderline
          (   (withDefAttr green $ center $ str
                "(withDefAttr green \n  $ center \n  $ str \"this\")"
              )
          <=> (withAttr red $ center $ str
                "(withDefAttr red \n  $ center \n  $ str \"this\")"
              )
          <=> (center $ str "(center \n  $ str \"this\")")
          )
        )

overGreenWithBlue :: Widget n
overGreenWithBlue =
  (str "withAttr blue \n  (overrideAttr\n    green magentaUnderline (")
    <=> (withAttr
          blue
          (overrideAttr
            green
            magentaUnderline
            (   (withDefAttr green $ center $ str
                  "(withDefAttr green \n  $ center \n  $ str \"this\")"
                )
            <=> (withAttr red $ center $ str
                  "(withDefAttr red \n  $ center \n  $ str \"this\")"
                )
            <=> (center $ str "(center \n  $ str \"this\")")
            )
          )
        )
overRedWithDefBlue :: Widget n
overRedWithDefBlue =
  (str "withDefAttr blue \n  (overrideAttr\n    green magentaUnderline (")
    <=> (withDefAttr
          blue
          (overrideAttr
            green
            magentaUnderline
            (   (withDefAttr green $ center $ str
                  "(withDefAttr green \n  $ center \n  $ str \"this\")"
                )
            <=> (withAttr red $ center $ str
                  "(withDefAttr red \n  $ center \n  $ str \"this\")"
                )
            <=> (center $ str "(center \n  $ str \"this\")")
            )
          )
        )
overInner :: Widget n
overInner =
  (str "\n\n ")
    <=> (overrideAttr
          red
          magentaUnderline
          (   ( withDefAttr green
              $ overrideAttr green magentaUnderline
              $ center
              $ str
                  "(withDefAttr green \n  $ overrideAttr\n     green magentaUnderline \n  $ center \n  $ str \"this\")"
              )
          <=> ( withAttr red
              $ overrideAttr red magentaUnderline
              $ center
              $ str
                  "(withDefAttr red \n  $ overrideAttr\n     red magentaUnderline\n  $ center \n  $ str \"this\")"
              )
          <=> (center $ str "(center \n  $ str \"this\")")
          )
        )

drawUI :: s -> [Widget n]
drawUI _ =
  [ (   border drawWithAttr
    <+> border drawWithDefAttr
    <+> border overGreen
    <+> border overRed
    <+> border overGreenWithBlue
    <+> border overRedWithDefBlue
    <+> border overInner
    )
  ]

handleEvent :: s -> BrickEvent n e -> EventM n (Next s)
handleEvent s e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> halt s
    _                    -> continue s
  _ -> continue s
