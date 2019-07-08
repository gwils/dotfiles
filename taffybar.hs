{-# language OverloadedStrings #-}

import           System.Taffybar
import           System.Taffybar.Hooks
import           System.Taffybar.Information.CPU
import           System.Taffybar.Information.Memory
import           System.Taffybar.SimpleConfig

import           System.Taffybar.Widget
import           System.Taffybar.Widget.Battery
import           System.Taffybar.Widget.DiskIOMonitor
import           System.Taffybar.Widget.FreedesktopNotifications
import           System.Taffybar.Widget.Generic.PollingBar
import           System.Taffybar.Widget.Generic.PollingGraph
import           System.Taffybar.Widget.MPRIS2
import           System.Taffybar.Widget.NetworkGraph
import           System.Taffybar.Widget.SimpleClock
import           System.Taffybar.Widget.SNITray
import           System.Taffybar.Widget.Workspaces
import           System.Taffybar.Widget.Util


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
      diskCfg = defaultGraphConfig { graphDataColors = [(0,0,1,1)]
                                   , graphLabel = Just "disk"
                                   }
      netCfg = defaultGraphConfig { graphDataColors = [(1,0,1,1)]
                                   , graphLabel = Just "net"
                                   }
      myWorkspacesConfig =
        defaultWorkspacesConfig
          { minIcons = 1
          , widgetGap = 0
          , showWorkspaceFn = hideEmpty
          }
      workspaces = workspacesNew myWorkspacesConfig
      layout = layoutNew defaultLayoutConfig
      windows = windowsNew defaultWindowsConfig
      clock = textClockNew Nothing "<span fgcolor='white'>%a %_d %b %H:%M:%S</span>" 1
      --pager = taffyPagerNew defaultPagerConfig
      note = notifyAreaNew defaultNotificationConfig
      mpris = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      disk = dioMonitorNew diskCfg 1 "nvme0n1p3"
      net = networkGraphNew netCfg Nothing
      tray = sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
      batt = textBatteryNew "  battery: $percentage$%"
      myConfig = defaultSimpleTaffyConfig
                       { barHeight = 20
                       , startWidgets = [ note]
                       , endWidgets = [ tray, batt, clock, mem, cpu, disk, net {-, mpris-} ]
                       , monitorsAction = return [0]
                       }
  dyreTaffybar $ toTaffyConfig myConfig
