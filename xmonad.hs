import qualified Data.Map as M
import Data.Ratio ((%))
import XMonad
import Graphics.X11.ExtraTypes.XF86
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.EasyMotion
import XMonad.Actions.FloatKeys
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowMenu
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.IfMax
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.Named
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedActions
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import System.IO

-- == Topics ==
topicItems :: [TopicItem]
topicItems =
  [ inHome   "web(1)" (spawn "google-chrome-stable")
  , inHome   "code(2)" (spawn "gnome-terminal")
  , inHome   "work(3)" (spawn "gnome-terminal")
  , noAction "4" "."
  , noAction "5" "."
  , noAction "6" "."
  , noAction "7" "."
  , inHome   "chat(8)" (spawn "discord")
  , inHome   "plex(9)" (spawn "plex")
  ]

myTopicConfig :: TopicConfig
myTopicConfig = def
  { topicDirs          = tiDirs    topicItems
  , topicActions       = tiActions topicItems
  , defaultTopicAction = const (pure ()) -- by default, do nothing
  , defaultTopic       = "1:Web"         -- fallback
  }

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt def goto

promptedShift :: X ()
promptedShift = workspacePrompt def $ windows . W.shift

-- == Layout ==
mySpacing = spacingRaw True (Border 3 3 3 3) True (Border 3 3 3 3) True
nmaster = 1
ratio = 1/2
delta = 3/100
magnification = 1.7

threecol = renamed [Replace "[3c]"]
         $ smartBorders
         $ mySpacing
         $ Mag.magnifiercz' magnification
         $ ThreeColMid nmaster delta ratio


tiled = renamed [Replace "[tall]"]
      $ mySpacing
      $ Mag.magnifiercz' magnification
      $ Tall nmaster delta ratio

full = renamed[Replace "[full]"]
     $ noBorders Full

fullif = renamed [Replace "[fif]"]
       $ IfMax 1 full tiled

onebig = renamed [Replace "[1b]"]
       $ smartBorders
       $ mySpacing
       $ Mag.magnifiercz' magnification
       $ OneBig (3/4) (3/4)

myLayoutHook = avoidStruts
             $ layouts
    where
        layouts = threecol ||| fullif ||| full ||| onebig

-- == Keybindings ==
myModMask = mod4Mask

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=terminus"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

-- any workspace but scratchpad
notSP = (return $ ("NSP" /=) . W.tag) :: X (WindowSpace -> Bool)
shiftAndView dir = findWorkspace getSortByIndex dir (WSIs notSP) 1
        >>= \t -> (windows . W.shift $ t) >> (windows . W.greedyView $ t)

myKeys conf = let
    subKeys str ks = subtitle str : mkNamedKeymap conf ks

    in
    subKeys "System"
    [ ("<XF86AudioLowerVolume>", addName "Volume down" $ spawn "amixer -D pulse set Master 2%-")
    , ("<XF86AudioRaiseVolume>", addName "Volume up"   $ spawn "amixer -D pulse set Master 2%+")
    , ("<XF86AudioMute>",        addName "Mute/Unmute" $ spawn "amixer -D pulse set Master toggle")
    , ("M-S-;",                  addName "Display off" $ spawn "sleep 2 && xset dpms force off")
    ] ^++^

    subKeys "Display"
    [ ("M-b", addName "Toggle struts" $ sendMessage ToggleStruts)
    ] ^++^

    subKeys "Windows"
    [ ("M-d",       addName "Duplicate window to all ws"   $ toggleCopyToAll)
    -- easy motion
    , ("M-f",       addName "Easy motion focus visible"    $ selectWindow def >>= (`whenJust` windows . W.focusWindow))
    -- window prompts
    , ("M-i",       addName "Open window menu"             $ windowMenu)
    , ("M-s",       addName "Goto window"                  $ gotoMenu)
    , ("M-S-s",     addName "Bring window"                 $ bringMenu)
    -- resizing and moving floating windows without a mouse
    , ("M-C-l",     addName "Resize float expand right"    $ withFocused (keysResizeWindow (10, 0) (0, 0)))
    , ("M-C-h",     addName "Resize float shrink left"     $ withFocused (keysResizeWindow (-10, 0) (0, 0)))
    , ("M-C-k",     addName "Resize float shrink up"       $ withFocused (keysResizeWindow (0, -10) (0, 0)))
    , ("M-C-j",     addName "Resize float expand down"     $ withFocused (keysResizeWindow (0, 10) (0, 0)))
    , ("M-C-<U>",   addName "Move float up"                $ withFocused (keysMoveWindow (0, -10)))
    , ("M-C-<D>",   addName "Move float down"              $ withFocused (keysMoveWindow (0, 10)))
    , ("M-C-<L>",   addName "Move float left"              $ withFocused (keysMoveWindow (-10, 0)))
    , ("M-C-<R>",   addName "Move float right"             $ withFocused (keysMoveWindow (10, 0)))
    , ("M-C-m",     addName "Toggle magnification"         $ sendMessage Mag.Toggle)
    , ("M-C-i",     addName "Increase magnification"       $ sendMessage Mag.MagnifyMore)
    , ("M-C-o",     addName "Decrease magnification"       $ sendMessage Mag.MagnifyLess)
    ] ^++^

    subKeys "Workspaces"
    [ ("M-a",    addName "Toggle last workspace" $ toggleWS' ["NSP"])
    , ("M-g",    addName "Goto workspace"        $ promptedGoto)
    , ("M-S-g",  addName "Send to workspace"     $ promptedShift)
    ] ^++^

    subKeys "Launchers"
    [ ("M-o",  addName "Rofi" $ spawn "rofi -theme purple -matching fuzzy -modi combi -show combi -combi-modi run,drun")
    , ("M-p",  addName "Rofi" $ spawn "rofi -theme purple -matching fuzzy -modi combi -show combi -combi-modi run,drun")
    ]
        where
            toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
                [] -> windows copyToAll

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = yellow . wrap "<[" "]>"
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myconfig = def
  { modMask = mod4Mask
  , layoutHook = myLayoutHook
  , terminal = "gnome-terminal"
  , borderWidth = 2
  , focusFollowsMouse = False
  -- focus AND pass click through
  , clickJustFocuses = True
  , focusedBorderColor = "#cd8b00"
  , workspaces = topicNames topicItems
  , manageHook = insertPosition End Newer
  , startupHook = setWMName "LG3D"
  }

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "/home/matt/.local/bin/xmobar" (pure myXmobarPP)) defToggleStrutsKey
     . addDescrKeys ((myModMask, xK_F1), showKeybindings) myKeys
     $ myconfig
