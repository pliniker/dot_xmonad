import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.TaffyPager
import System.Taffybar.Pager
import System.Taffybar.Battery
import System.Taffybar.NetMonitor

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

blank _ = ""

small s = "<span font_size='x-small'>" ++ s ++ "</span>"

pagerConfig   = PagerConfig
                { activeWindow     = colorize "#f8f8f8" "black" . escape . shorten 40
                , activeLayout     = colorize "DarkOrange" "black" . wrap "[" "]" . escape
                , activeWorkspace  = colorize "black" "LightSkyBlue4" . wrap "[" "]" . escape
                , hiddenWorkspace  = colorize "black" "SkyBlue4" . small . escape
                , emptyWorkspace   = colorize "#777777" "black" . small . escape
                , visibleWorkspace = wrap "(" ")" . escape
                , urgentWorkspace  = colorize "#f8f8f8" "red4" . escape
                , widgetSep        = " / "
                }

main = do
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Nothing
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Nothing
                                  }
  let clock = textClockNew Nothing "<span fgcolor='#f8f8f8'>%a %b %_d %H:%M</span>" 1
      pager = taffyPagerNew pagerConfig
      mem = pollingGraphNew memCfg 2 memCallback
      cpu = pollingGraphNew cpuCfg 2 cpuCallback
      batt = batteryBarNew defaultBatteryConfig 30
      tray = systrayNew
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                        , endWidgets = [ clock, batt, tray, mem, cpu ]
                                        , monitorNumber = 0
                                        }
