{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Control.Arrow ((***), second)
import Control.Monad
import Data.Maybe

import System.Taffybar.Hooks.PagerHints (pagerHints)
import System.Exit

import XMonad
import XMonad.Actions.UpdatePointer
import XMonad.Core                      (XConfig (..))
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops        (ewmh)
import XMonad.Hooks.ManageDocks         (avoidStruts, docks, manageDocks)
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.ManageHook                ((<+>))
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig             (additionalKeys)
import XMonad.Util.Loggers
import XMonad.Actions.GridSelect

main :: IO ()
main =
  -- docks: add dock (panel)
  -- ewmh - lets xmonad talk to panels
  -- pagerhints: add support for Taffybar's current layout and workspaces hints
  xmonad . docks . ewmh . pagerHints $ defaultConfig
    {
      -- avoid the "strut" (where docks live)
      layoutHook = avoidStruts landscapeLayoutHook
    , logHook = updatePointer (0.5, 0.5) (0,0) >> takeTopFocus >> setWMName "LG3D"
      -- let xmonad manage docks
    , manageHook = manageDocks <+> manageHook defaultConfig
    , modMask = mod4Mask
    , terminal = "terminator"
    } `additionalKeys` [
    --
    -- KEYS
    --
      ((mod4Mask,               xK_e     ), spawn "thunderbird")
    , ((mod4Mask,               xK_f     ), spawn "firefox")
    , ((mod4Mask,               xK_g     ), spawn "google-chrome")
    , ((mod4Mask,               xK_s     ), spawn "sublime")
    , ((mod4Mask .|. shiftMask, xK_z     ), spawn "xscreensaver-command -lock")
    ]

--
-- LAYOUT
--
newtype Flip l a = Flip (l a) deriving (Show, Read)

landscapeLayoutHook = tiled ||| flipTiled ||| GridRatio (16/10) ||| Mirror tiled ||| Full
  where
    tiled = Tall nmaster delta ratio
    flipTiled = Flip tiled
    nmaster = 1
    ratio = 1/2
    delta = 3/100

portraitLayoutHook = Mirror tiled ||| Full ||| GridRatio (10/16)
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100


instance LayoutClass l a => LayoutClass (Flip l) a where
  runLayout (W.Workspace i (Flip l) ms) r =
    (map (second flipRect) *** fmap Flip)
      `fmap` runLayout (W.Workspace i l ms) (flipRect r)
    where
      screenWidth = fromIntegral $ rect_width r
      flipRect (Rectangle rx ry rw rh) = Rectangle (screenWidth - rx - (fromIntegral rw)) ry rw rh
  handleMessage (Flip l) = fmap (fmap Flip) . handleMessage l
  description (Flip l) = "Flip "++ description l

--
-- Cursor follows focus
--
takeTopFocus ::
  X ()
takeTopFocus =
  withWindowSet $ maybe (setFocusX =<< asks theRoot) takeFocusX . W.peek

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

showws ::
  String
  -> Bool
showws x =
  any (`elem` x) ['a'..'z']
