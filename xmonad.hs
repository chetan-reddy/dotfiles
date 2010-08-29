import XMonad
import qualified Data.Map as Map

import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W
import Monad
import Data.Monoid (All (All))

import Graphics.X11.ExtraTypes.XF86

import XMonad.Layout.Tabbed
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation

import XMonad.Layout.BoringWindows

myLayout = windowNavigation $ subTabbed $ boringWindows $
           Tall 1 (3/100) (1/2)

main = xmonad $ defaultConfig
        { terminal = "urxvtc" 
        , modMask = mod3Mask 
        , keys = \c -> myKeys `Map.union` keys defaultConfig c
        , handleEventHook = evHook
        , layoutHook = myLayout
 }
        
modm = mod3Mask

myKeys = Map.fromList $
         [ ((mod3Mask,  xK_Menu), return() ),
           ((0, xK_Menu), return() ),
           ((mod3Mask, xK_t), spawn "iceweasel"),
           ((mod3Mask .|. shiftMask, xK_o), spawn "sudo -u sandbox -i '/home/sandbox/opera-10.61-6430.x86_64.linux/opera && cleanup'"),
           --((mod3Mask .|. shiftMask, xK_o), spawn "xrandr -o left"),
           ((mod3Mask .|. shiftMask, xK_x), spawn "xrandr -o left"),
           ((mod3Mask, xK_x), spawn "xrandr -o normal")
         , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
         , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
         , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
         , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)
         , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
         , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
         , ((modm .|. controlMask, xK_period), onGroup W.focusUp')
         , ((modm .|. controlMask, xK_comma), onGroup W.focusDown')
         , ((modm, xK_j), focusDown)
         , ((modm, xK_k), focusUp)
--           ((0, 213), spawn "sudo s2disk")
--           ((mod3Mask, xK_r), spawn "xrandr -o normal")
         ]

-- Helper functions to fullscreen the window
fullFloat, tileWin :: Window -> X ()
fullFloat w = windows $ W.float w r
    where r = W.RationalRect 0 0 1 1
tileWin w = windows $ W.sink w

evHook :: Event -> X All
evHook (ClientMessageEvent _ _ _ dpy win typ dat) = do
  state <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  isFull <- runQuery isFullscreen win

  -- Constants for the _NET_WM_STATE protocol
  let remove = 0
      add = 1
      toggle = 2

      -- The ATOM property type for changeProperty
      ptype = 4 

      action = head dat

  when (typ == state && (fromIntegral fullsc) `elem` tail dat) $ do
    when (action == add || (action == toggle && not isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace [fromIntegral fullsc]
         fullFloat win
    when (head dat == remove || (action == toggle && isFull)) $ do
         io $ changeProperty32 dpy win state ptype propModeReplace []
         tileWin win

  -- It shouldn't be necessary for xmonad to do anything more with this event
  return $ All False

evHook _ = return $ All True
