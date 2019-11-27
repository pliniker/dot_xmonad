-- the battery monitor will cause a fatal error if upower is not installed

import System.Information.Memory
import System.Taffybar
import System.Taffybar.Battery
import System.Taffybar.CPUMonitor
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.FSMonitor
import System.Taffybar.NetMonitor
import System.Taffybar.Pager (colorize, escape)
import System.Taffybar.SimpleClock
import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.Text.MemoryMonitor
import System.Taffybar.Weather
import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.WorkspaceHUD

memCallback = do
  mi <- parseMeminfo
  return $ map ((/memoryTotal mi) . ($ mi)) [ memoryBuffer, memoryCache ]

main = do
  let cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
                                  , graphLabel = Just "cpu"
                                  }
      memCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
                                  , graphLabel = Just "mem"
                                  }
      clock = textClockNew Nothing "<span fgcolor='lightgray'>%a %b %_d %H:%M</span>" 1
      pagerconfig = defaultPagerConfig { emptyWorkspace = colorize "lightgray" "" . escape }
      pager = taffyPagerHUDNew pagerconfig (hudFromPagerConfig pagerconfig)
      tray = systrayNew
      cpu = cpuMonitorNew cpuCfg 2.0 "cpu"
      mem = pollingGraphNew memCfg 4.0 memCallback
      battery = batteryBarNew defaultBatteryConfig 60.0
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
                                        , endWidgets = [ tray
                                                       , battery
                                                       , clock
                                                       , mem
                                                       , cpu
                                                       ]
                                        }
