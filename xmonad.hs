import System.IO
import System.Exit(ExitCode(ExitSuccess), exitWith)

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as W
import XMonad.Layout.IndependentScreens
import XMonad.Util.WorkspaceCompare

import System.Taffybar.Hooks.PagerHints (pagerHints)

import DBus
import DBus.Client


-- Tags/Workspaces
wsNames = map show [1 .. 9 :: Int]

-- Layouts
myLayoutHook = avoidStruts $ smartBorders ( tiled ||| mtiled ||| full )
  where
    full = named "X" $ Full
    mtiled = named "M" $ Mirror tiled
    tiled = named "T" $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
    -- sets default tile as: Tall nmaster (delta) (golden ratio)

-- Window management
myManageHook = composeAll
  [ className =? "Vlc" --> doFloat
  , className =? "Gimp" --> doFloat
  , className =? "XCalc" --> doFloat
  , className =? "XMessage" --> doFloat
  ]

-- Key bindings
myKeys = [ ("M-b", sendMessage ToggleStruts)
         , ("C-M1-l", spawn "gnome-screensaver-command -l")
         , ("C-M-M1-q", spawn "xmonad --recompile && xmonad --restart")
         , ("C-M-S-q", io (exitWith ExitSuccess))
         ]

-- Workspace keys
wsKeys = [((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
              | (i, k) <- zip wsNames [xK_1 .. xK_9]
              , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
         ]

-- Remove these keybindings
remKeys = [ "M-q"
          , "M-S-q"
          ]

-- Configuration
conf monitors dbus = defaultConfig
  { modMask = mod4Mask
  , terminal = "xfce4-terminal"
  , borderWidth = 3
  , normalBorderColor = "#778877"
  , focusedBorderColor = "#AA3333"
  , handleEventHook = fullscreenEventHook
  , workspaces = withScreens monitors wsNames
  , layoutHook = myLayoutHook
  , manageHook = manageDocks <+> myManageHook
                 <+> manageHook defaultConfig
  }
  `additionalKeysP` myKeys
  `additionalKeys` wsKeys
  `removeKeysP` remKeys

main = do
  dbus <- connectSession
  monitors <- countScreens
  spawn "~/.xmonad/xsession"
  xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ pagerHints $ conf monitors dbus
