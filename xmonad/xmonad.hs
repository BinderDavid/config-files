import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.LayoutModifier
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import System.IO
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------
-- Key bindings.
------------------------------------------------------------------------
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
      -- Mod + Shift + Enter:
      -- Launch a terminal.
      ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

      -- Mod + p:
      -- Launch dmenu.
    , ((modm,               xK_p     ), spawn "dmenu_run")

      -- Mod + Shift + c:
      -- Close focused window.
    , ((modm .|. shiftMask, xK_c     ), kill)

      -- Mod + Space:
      -- Rotate through the available layout algorithms.
    , ((modm,               xK_space ), sendMessage NextLayout)

      -- Mod + Shift + Space:
      -- Reset the layouts on the current workspace to default.
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

      -- Mod + n:
      -- Resize viewed windows to the correct size.
    , ((modm,               xK_n     ), refresh)

      -- Mod + Tab:
      -- Move focus to the next window.
    , ((modm,               xK_Tab   ), windows W.focusDown)

      -- Mod + j:
      -- Move focus to the next window.
    , ((modm,               xK_j     ), windows W.focusDown)

      -- Mod + k:
      -- Move focus to the previous window.
    , ((modm,               xK_k     ), windows W.focusUp  )

      -- Mod + m:
      -- Move focus to the master window.
    , ((modm,               xK_m     ), windows W.focusMaster  )

      -- Mod + Enter:
      -- Swap the focused window and the master window.
    , ((modm,               xK_Return), windows W.swapMaster)

      -- Mod + Shift + j:
      -- Swap the focused window with the next window.
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

      -- Mod + Shift + k:
      -- Swap the focused window with the previous window.
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

      -- Mod + h:
      -- Shrink the master area.
    , ((modm,               xK_h     ), sendMessage Shrink)

      -- Mod + h:
      -- Expand the master area.
    , ((modm,               xK_l     ), sendMessage Expand)

      -- Mod + t:
      -- Push window back into tiling.
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

      -- Mod + ,:
      -- Increment the number of windows in the master area.
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

      -- Mod + .:
      -- Deincrement the number of windows in the master area.
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

      -- Mod + b:
      -- Toggle the status bar gap
      -- Use this binding with avoidStruts from Hooks.ManageDocks.
      -- See also the statusBar function from Hooks.DynamicLog.
      -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

      -- Mod + Shift + q:
      -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

      -- Mod + q:
      -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    ]
    ++
    -- Mod + [1..9]:
    -- Switch to workspace N.
    [((modm, k), windows $ W.greedyView i) |
     (k,i) <- zip [xK_1 .. xK_9] (XMonad.workspaces conf)]
    ++
    -- Mod + Shift + [1..9]:
    -- Move client to workspace N.
    [((modm .|. shiftMask, k), windows $ W.shift i) |
     (k,i) <- zip [xK_1 .. xK_9] (XMonad.workspaces conf)]
    ++
    -- Mod + [w,e,r]:
    -- Switch to physical/Xinerama screens 1, 2, or 3.
    [((modm, k), screenWorkspace sc >>= flip whenJust (windows . W.view)) |
     (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]]
    ++
    -- Mod + [w,e,r]:
    -- Move client to screen 1, 2, or 3.
    [((modm .|. shiftMask, k), screenWorkspace sc >>= flip whenJust (windows . W.shift)) |
     (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
------------------------------------------------------------------------
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
  [
    -- Mod + button1:
    -- Set the window to floating mode and move by dragging.
    ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                     >> windows W.shiftMaster))

    -- Mod + button2:
    -- Raise the window to the top of the stack.
  , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
  
    -- Mod + button3:
    -- Set the window to floating mode and resize by dragging.
  , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
  ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
------------------------------------------------------------------------
myLayout :: Choose Tall (Choose (Mirror Tall) Full) a
myLayout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 2/3

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
------------------------------------------------------------------------
myManageHook :: ManageHook
myManageHook = manageDocks <+> manageHook def

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
------------------------------------------------------------------------
myEventHook :: Event -> X All
myEventHook = fullscreenEventHook --Fixes Fullscreen from browser
              <+> docksEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
------------------------------------------------------------------------
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ xmobarPP
                                     { ppOutput = hPutStrLn h
                                     , ppTitle = xmobarColor "green" "" . shorten 50
                                     }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
  spawn "xrandr --output HDMI-1 --primary"
  spawn "xrandr --output VGA-1  --rotate left --left-of HDMI-1"
  spawn "xrandr --output HDMI-1 --pos 1080x600"
  setWMName "LG3D"


------------------------------------------------------------------------
-- XMonad Config
------------------------------------------------------------------------

defaults :: Handle
         -> XConfig (ModifiedLayout AvoidStruts (Choose Tall (Choose (Mirror Tall) Full)))
defaults xmproc = XConfig
  {
    -- Border colors for unfocused and focused windows, respectively.
    normalBorderColor  = "#dddddd"
  , focusedBorderColor = "#ff0000"
    -- The preferred terminal program, which is used in a binding below and by
    -- certain contrib modules.
  , terminal           = "x-terminal-emulator"
  , layoutHook         = avoidStruts  $  layoutHook def --change eventually
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
    -- The default number of workspaces (virtual screens) and their names.
    -- By default we use numeric strings, but any string may be used as a
    -- workspace name. The number of workspaces is determined by the length
    -- of this list.
    --
    -- A tagging example:
    --
    -- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
  , workspaces         = ["1","2","3","4","5","6","7","8","9"]
    -- Bind ModKey to Windows key.
  , modMask            = mod4Mask
  , keys               = myKeys
  , mouseBindings      = myMouseBindings
    -- Width of the window border in pixels.
  , borderWidth        = 2
  , logHook            = myLogHook xmproc
  , startupHook        = myStartupHook
    -- Whether focus follows the mouse pointer.
  , focusFollowsMouse  = False
    -- Whether clicking on a window to focus also passes the click to the window.
  , clickJustFocuses   = False
  , clientMask         = structureNotifyMask .|. enterWindowMask .|. propertyChangeMask
  , rootMask           = substructureRedirectMask .|. substructureNotifyMask
                         .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask
                         .|. buttonPressMask
  , handleExtraArgs    = \xs conf -> case xs of
      [] -> return conf
      _ -> fail ("unrecognized flaggs:" ++ show xs)
  }

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
------------------------------------------------------------------------
mybar :: String
mybar = "xmobar ~/.xmobarrc"

main :: IO ()
main = do
  xmproc <- spawnPipe mybar 
  xmonad (defaults xmproc) 

