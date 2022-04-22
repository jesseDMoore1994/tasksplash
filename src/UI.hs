{-# LANGUAGE OverloadedStrings #-}

module UI
  ( run
  ) where

import Brick
  ( App(..)
  , AttrName
  , BrickEvent(..)
  , EventM
  , Next
  , Widget
  , (<+>)
  , attrMap
  , attrName
  , bg
  , continue
  , defaultMain
  , fg
  , halt
  , neverShowCursor
  , padAll
  , str
  , vBox
  , withBorderStyle
  )
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Core (withAttr)
import Control.Monad (void)
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T
import Graphics.Vty (Color(ISOColor), blue, defAttr, green)

data State =
  State
    {
    }

type Name = ()

githubPanelAttrName :: AttrName
githubPanelAttrName = attrName "githubPanel"

githubPanelItemAttrName :: AttrName
githubPanelItemAttrName = attrName "githubPanelItem"

jiraPanelAttrName :: AttrName
jiraPanelAttrName = attrName "jiraPanel"

jiraPanelItemAttrName :: AttrName
jiraPanelItemAttrName = attrName "jiraPanelItem"

initialState :: State
initialState = State {}

handleEvent :: State -> BrickEvent Name e -> EventM Name (Next State)
handleEvent s _ = halt s

draw :: State -> [Widget Name]
draw s = [center . padAll 1 $ drawJira s <+> drawGithub s]

fakeGithubLinks :: String
fakeGithubLinks =
  unlines
    [ "https://www.github.com/issues/1"
    , "https://www.github.com/issues/2"
    , "https://www.github.com/issues/3"
    ]

fakeJiraCns :: String
fakeJiraCns = unlines ["CN-725334", "CN-734503", "CN-722223"]

drawGithub :: State -> Widget Name
drawGithub s =
  withAttr "githubPanel" $
  withBorderStyle unicodeBold $
  borderWithLabel (str "Github") $
  hCenter $ padAll 1 $ withAttr "githubPanelItem" $ str fakeGithubLinks

drawJira :: State -> Widget Name
drawJira s =
  withAttr "jiraPanel" $
  withBorderStyle unicodeBold $
  borderWithLabel (str "Jira") $
  hCenter $ padAll 1 $ withAttr "jiraPanelItem" $ str fakeJiraCns

app :: App State e ()
app =
  App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap =
        const $
        attrMap
          defAttr
          [ (githubPanelAttrName, fg green)
          , (githubPanelItemAttrName, bg green)
          , (jiraPanelAttrName, fg blue)
          , (jiraPanelItemAttrName, bg blue)
          ]
    }

run :: CT.Config -> IO ()
run cfg = void (defaultMain app initialState)
