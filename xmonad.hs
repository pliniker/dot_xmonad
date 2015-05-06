import System.IO

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

import System.Taffybar.XMonadLog (dbusLogWithPP, taffybarDefaultPP,
                                   taffybarColor, taffybarEscape)

import DBus
import DBus.Client


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
            , logHook = dbusLogWithPP dbus myXmobarPP
            }
            `additionalKeysP` myKeys
            `additionalKeys` wsKeys

-- Tags/Workspaces
wsNames = map show [1 .. 9 :: Int]

-- Layouts
-- the default layout is fullscreen with smartborders applied to all
myLayoutHook = avoidStruts $ smartBorders ( tiled ||| mtiled ||| full )
  where
    full = named "X" $ Full
    mtiled = named "M" $ Mirror tiled
    tiled = named "T" $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
    -- sets default tile as: Tall nmaster (delta) (golden ratio)

-- Window management
myManageHook = composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "Vlc" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "XCalc" --> doFloat
    ]

-- Statusbar
blank _ = ""
myXmobarPP = taffybarDefaultPP
   {
     ppCurrent = taffybarColor "#f8f8f8" "DodgerBlue4" . wrap " " " "
   , ppVisible = taffybarColor "#f8f8f8" "LightSkyBlue4" . wrap " " " "
   , ppUrgent  = taffybarColor "#f8f8f8" "red4" . wrap " " " "
   , ppHidden = blank
   , ppHiddenNoWindows = blank
   , ppWsSep = " : "
   , ppLayout  = taffybarColor "DarkOrange" "" . wrap " [" "] "
   , ppTitle   = taffybarColor "#ffffff" "" . shorten 150
   }

-- Key bindings
myKeys = [ ("M-b", sendMessage ToggleStruts)
         , ("C-M1-l", spawn "xscreensaver-command --lock")
         ]

-- Workspace keys
wsKeys = [
           ((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
              | (i, k) <- zip wsNames [xK_1 .. xK_9]
              , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
         ]

main = do
  dbus <- connectSession
  tools <- spawnPipe "~/.xmonad/xsession"
  monitors <- countScreens
  xmonad $ withUrgencyHook NoUrgencyHook $ conf monitors dbus
