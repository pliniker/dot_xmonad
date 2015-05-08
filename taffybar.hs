import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.MPRIS
import System.Taffybar.XMonadLog

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

main = do
  let memCfg = defaultGraphConfig { graphDataColors = [(1, 0, 0, 1)]
                                  , graphLabel = Just "mem"
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
                                                      , (1, 0, 1, 0.5)
                                                      ]
                                  , graphLabel = Just "cpu"
                                  }
  let clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
      note = notifyAreaNew defaultNotificationConfig
      xmlog = xmonadLogNew
      wea = weatherNew (defaultWeatherConfig "KAVL") 10
      mpris = mprisNew
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 1 cpuCallback
      tray = systrayNew
  defaultTaffybar defaultTaffybarConfig { startWidgets = [ xmlog, note ]
                                        , endWidgets = [ tray, wea, clock,
                                                         mem, cpu, mpris ]
                                        , monitorNumber = 0
                                        }
