{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog hiding (dzen)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Loggers
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Roledex
import XMonad.Layout.Dishes
import XMonad.Layout.Spiral
import XMonad.Layout.StackTile
import XMonad.Layout
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import Control.Arrow ((***), second)
import Control.Monad
import Data.Maybe
import XMonad.Actions.GridSelect

main ::
  IO ()
main =
  xmonad'

xmonad' ::
  IO ()
xmonad' =
  xmonad =<< dzen

ws :: [String]
ws =
   ["1:code", "2:shell", "3:browser", "4:comms", "5:music", "6:diff", "7", "8", "9"]

showws ::
  String
  -> Bool
showws x =
  any (`elem` x) ['a'..'z']

wkspHasNoWindows ::
  Query Bool
  -> WindowSpace
  -> X Bool
wkspHasNoWindows q w =
  fmap null (filterM (runQuery q) (W.integrate' $ W.stack w))

firstWkspWithoutWindows ::
  Query Bool
  -> X (Maybe WorkspaceId)
firstWkspWithoutWindows q =
  let keepWindow = filter (const True) -- keep them all
  in do ws <- gets windowset
        res <- filterM (wkspHasNoWindows q) (keepWindow $ W.workspaces ws)
        return (fmap W.tag . listToMaybe $ res)

putWhereNoWindow ::
  Query Bool
  -> ManageHook
putWhereNoWindow q =
  do mid <- liftX (firstWkspWithoutWindows q)
     case mid of
          Nothing -> idHook
          (Just id) -> doShift id

dzen =
  let flags = "-ta r -e 'onstart=lower' -fg '#FFff00' -bg '#0000ff' -h '16' -fn '-schumacher-clean-bold-r-normal--12-120-75-75-c-60-iso646.1991-irv'" -- Use output of xlsfonts to set font (-fn)
      toggleStrutsKey XConfig{modMask = modm} = (modm, xK_b )
  in statusBar ("dzen2 " ++ flags) pp toggleStrutsKey conf

takeFocusX ::
  Window
  -> X ()
takeFocusX w =
  withWindowSet . const $ do
    dpy       <- asks display
    wmtakef   <- atom_WM_TAKE_FOCUS
    wmprot    <- atom_WM_PROTOCOLS
    protocols <- io $ getWMProtocols dpy w
    when (wmtakef `elem` protocols) $
      io . allocaXEvent $ \ev -> do
          setEventType ev clientMessage
          setClientMessageEvent ev w wmprot 32 wmtakef currentTime
          sendEvent dpy w False noEventMask ev

takeTopFocus ::
  X ()
takeTopFocus =
  withWindowSet $ maybe (setFocusX =<< asks theRoot) takeFocusX . W.peek

portraitLayoutHook = Mirror tiled ||| Full ||| GridRatio (10/16)
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

--landscapeLayoutHook = layoutHook defaultConfig ||| GridRatio (16/10)
landscapeLayoutHook = tiled ||| flipTiled ||| GridRatio (16/10) ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    flipTiled = Flip tiled
    nmaster = 1
    ratio = 1/2
    delta = 3/100

newtype Flip l a = Flip (l a) deriving (Show, Read)

instance LayoutClass l a => LayoutClass (Flip l) a where
  runLayout (W.Workspace i (Flip l) ms) r =
    (map (second flipRect) *** fmap Flip)
      `fmap` runLayout (W.Workspace i l ms) (flipRect r)
    where
      screenWidth = fromIntegral $ rect_width r
      flipRect (Rectangle rx ry rw rh) = Rectangle (screenWidth - rx - (fromIntegral rw)) ry rw rh
  handleMessage (Flip l) = fmap (fmap Flip) . handleMessage l
  description (Flip l) = "Flip "++ description l


conf =
  defaultConfig {
    layoutHook         = landscapeLayoutHook
  , modMask            = mod4Mask
  , borderWidth        = 4
  , workspaces         = ws
  , keys               = k
  , focusedBorderColor = "#ff0000"
  , terminal           = "/usr/bin/terminator"
  , logHook            = dynamicLogWithPP pp >> updatePointer (Relative 0.5 0.5) >> takeTopFocus >> setWMName "LG3D"  
  , manageHook         = let windowAction a w = withDisplay $ \d -> io $ a d w
                             windowActionHook a = ask >>= \w -> liftX (a w) >> idHook
                             isDockWindow = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DOCK"
                             isDesktopWindow = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DESKTOP"
                             lowerDesktopWindows = isDesktopWindow --> windowActionHook (windowAction lowerWindow)
                             raiseDockWindows = isDockWindow --> windowActionHook (windowAction raiseWindow)
                             stackWindows = lowerDesktopWindows <+> raiseDockWindows
                             unfloat = ask >>= doF . W.sink
                         in composeAll [
                                         stackWindows <+> manageHook defaultConfig
                                       , className =? "rdesktop" --> unfloat
                                       , className =? "tsclient" --> unfloat
                                       , className =? "Tsclient" --> unfloat
                                       , className =? "gimp"     --> unfloat
                                       , className =? "Gimp"     --> unfloat
                                       , isFullscreen --> doFullFloat
                                       , putWhereNoWindow (title =? "foobar")
                                       ]
  }

pp ::
  PP
pp =
  defaultPP {
      ppHiddenNoWindows = \wsId -> if showws wsId then pad wsId else []
    , ppHidden          = dzenColor "#999999"  "#262626" . pad
    , ppCurrent         = dzenColor "#262626" "#666666" . pad
    , ppUrgent          = dzenColor "red"    "yellow"
    , ppSep             = " | "
    , ppWsSep           = ""
    , ppTitle           = shorten 180
    , ppOrder           = \(ws:l:t:exs) -> [t,l,ws]++exs
    , ppExtras          = [ loadAvg , date "%A %d %b %H:%M"]
    }


k ::
  XConfig Layout
  -> M.Map (KeyMask, KeySym) (X ())
k conf@(XConfig {modMask = modMask}) =
  M.fromList $
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ terminal conf)

    -- launch dmenu
    , ((modMask,               xK_Escape), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- launch gmrun
    , ((modMask .|. shiftMask, xK_p     ), spawn "xfce4-appfinder")

    -- launch browser
    , ((modMask,               xK_f     ), spawn "firefox")

    -- launch editor
    , ((modMask,               xK_e     ), spawn "emacs")
    , ((modMask,               xK_u     ), spawn "subl")

    -- media player
    , ((modMask,               xK_r     ), spawn "banshee")

    -- launch firefox with gmail
    , ((modMask,               xK_c     ), spawn "firefox \"https://gmail.com\"")

    -- launch chrome
    , ((modMask,               xK_g     ), spawn "google-chrome")

    -- launch nautilus
    , ((modMask,               xK_Home  ), spawn "thunar")

    -- launch nautilus at ~/Desktop
    , ((modMask .|. shiftMask, xK_Home  ), spawn "thunar \"Desktop\"")

    -- lock screen
    , ((modMask,               xK_End   ), spawn "xflock4")


      -- shutdown
    , ((modMask .|. shiftMask, xK_End   ), spawn "xfce-session-logout --halt")

    -- close focused window
    , ((modMask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)

    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    , ((modMask              , xK_b), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
      [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
