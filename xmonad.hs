
import XMonad
import XMonad.Actions.CycleWindows -- classic alt-tab
import XMonad.Actions.CycleWS      -- cycle thru WS', toggle last WS
import XMonad.Hooks.DynamicLog     -- statusbar
import XMonad.Hooks.EwmhDesktops   -- fullscreenEventHook fixes chrome fullscreen
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Hooks.UrgencyHook    -- window alert bells
import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.NoBorders     -- smart borders on solo clients
import XMonad.Util.EZConfig        -- append key/mouse bindings
import XMonad.Util.Run(spawnPipe)  -- spawnPipe and hPutStrLn
import qualified XMonad.StackSet as W
import XMonad.Layout.IndependentScreens -- dwm style workspaces
import System.IO                   -- hPutStrLn scope


conf status monitors = defaultConfig
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
            , logHook = myLogHook status
            }
            `additionalKeysP` myKeys
            `additionalMouseBindings` myButtons
            `additionalKeys` wsKeys

main = do
        status <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
        tools <- spawnPipe "~/.xmonad/xsession"
        monitors <- countScreens
        xmonad $ withUrgencyHook NoUrgencyHook $ conf status monitors

-- Tags/Workspaces
--
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
--
myManageHook = composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "Vlc" --> doFloat
    , className =? "Gimp" --> doFloat
    , className =? "XCalc" --> doFloat
    , className =? "stalonetray" --> doIgnore
    ]

-- Statusbar
--
myLogHook h = dynamicLogWithPP $ myXmobarPP { ppOutput = hPutStrLn h }

myXmobarPP = xmobarPP
    { ppCurrent = xmobarColor "#3399ff" "" . wrap " " " "
--    , ppHidden = xmobarColor "#dddddd" "" . wrap " " " "
--    , ppHiddenNoWindows = xmobarColor "#777777" "" . wrap " " " "
    , ppUrgent = xmobarColor "#ff0000" "" . wrap " " " "
    , ppSep = " "
    , ppWsSep = "/"
    , ppLayout = xmobarColor "#aaaaaa" "" . wrap "·" "·"
    , ppTitle = xmobarColor "#ffffff" "" . shorten 25
    }

-- Key bindings
--
myKeys = [ ("M-b"        , sendMessage ToggleStruts              ) -- toggle the status bar gap
         , ("M1-<Tab>"   , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab ) -- classic alt-tab behaviour
         , ("M-<Tab>"    , toggleWS                              ) -- toggle last workspace (super-tab)
         , ("M-<Right>"  , nextWS                                ) -- go to next workspace
         , ("M-<Left>"   , prevWS                                ) -- go to prev workspace
         , ("M-S-<Right>", shiftToNext                           ) -- move client to next workspace
         , ("M-S-<Left>" , shiftToPrev                           ) -- move client to prev workspace
         , ("C-M1-l"     , spawn "xscreensaver-command --lock"   ) -- lock screen
         ]

-- Workspace keys
wsKeys = [
           ((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
              | (i, k) <- zip wsNames [xK_1 .. xK_9]
              , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
         ]

-- Mouse bindings
--
myButtons = [ ((0, 8), (\_ -> prevWS )) -- cycle workspaces
            , ((0, 9), (\_ -> nextWS )) -- with thumb buttons
            ]

-- vim:sw=4 sts=4 ts=4 tw=0 et ai
