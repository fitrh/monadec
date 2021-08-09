import Data.Monoid
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.MultiColumns
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.Loggers

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

terminalCmd = "st -f 'SF Mono:size=10'"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
altKey :: KeyMask
altKey = mod1Mask

superKey :: KeyMask
superKey = mod4Mask

shiftKey :: KeyMask
shiftKey = shiftMask

ctrlKey :: KeyMask
ctrlKey = controlMask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = [ "  \59285  "
                  , "  \63080  "
                  , "  \63288  "
                  , "  \58879  "
                  , "  \63256  "
                  , "  \63437  "
                  , "  \61574  "
                  , "  \58901  "
                  , "  \62866  "
                  ]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#1a1b26"
myFocusedBorderColor = "#232433"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- close focused window
    , ((modm .|. shiftMask, xK_c), kill)

     -- Rotate through the available layout algorithms
    , ((modm, xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm, xK_n), refresh)

    -- Move focus to the next window
    , ((modm, xK_Tab), windows W.focusDown)

    -- Move focus to the next window
    , ((modm, xK_j), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm, xK_k), windows W.focusUp)

    -- Move focus to the master window
    , ((modm, xK_m), windows W.focusMaster)

    -- Swap the focused window and the master window
    , ((modm, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown)

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp)

    -- Cycle between visibel workspaces
    , ((altKey .|. shiftKey, xK_bracketright), moveTo Next NonEmptyWS)
    , ((altKey .|. shiftKey, xK_bracketleft), moveTo Prev NonEmptyWS)
    -- Move focused window between workspaces
    , ((modm .|. shiftKey, xK_bracketright), shiftToNext >> nextWS)
    , ((modm .|. shiftKey, xK_bracketleft ), shiftToPrev >> prevWS)

    -- Shrink the master area
    , ((modm, xK_h), sendMessage Shrink)

    -- Expand the master area
    , ((modm, xK_l), sendMessage Expand)

    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q), io exitSuccess)

    -- Restart xmonad
    , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
        >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> Flex.mouseResizeWindow w
        >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((modm, button4), \w -> moveTo Next NonEmptyWS)
    , ((modm, button5), \w -> moveTo Prev NonEmptyWS)
    , ((modm .|. shiftKey, button4), \w -> focus w
        >> shiftToNext >> nextWS)
    , ((modm .|. shiftKey, button5), \w -> focus w
        >> shiftToPrev >> prevWS)
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
--
withGaps i = spacingRaw False (Border i 0 i 0) True (Border 0 i 0 i) True

leftStack =
    renamed [Replace "[]="] $
    withGaps 8 $
    Tall 1 (1/100) (1/2)

bottomStack =
    renamed [Replace "TTT"] $
    Mirror leftStack

centeredFloatingMaster =
    renamed [Replace ">M>"] $
    withGaps 8 $
    centerMaster (multiCol [1] 1 (-0.5) (-0.5))

centeredMaster =
    renamed [Replace "|M|"] $
    withGaps 8 $
    ThreeColMid 1 (1/100) (1/2)

grid =
    renamed [Replace ":::"] $
    withGaps 8 Grid

tab =
    renamed [Replace "---"] $
    withGaps 8 $
    tabbed shrinkText def

monocle =
    renamed [Replace "[M]"] $
    withGaps 8 Full

floating =
    renamed [Replace "><>"] simplestFloat

myLayout =
    leftStack
    ||| bottomStack
    ||| grid
    ||| centeredMaster
    ||| centeredFloatingMaster
    ||| tab
    ||| monocle
    ||| floating

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
--
myManageHook = composeAll
    [
      className =? "codespace" --> doShift (myWorkspaces !! 1)
    , className =? "discord" --> doShift (myWorkspaces !! 6) <+> doCenterFloat
    , className =? "File Manager" --> doShift (myWorkspaces !! 3) <+> doCenterFloat
    , className =? "firefoxdeveloperedition" --> doShift (myWorkspaces !! 2)
    , (className =? "firefoxdeveloperedition" <&&> role =? "PictureInPicture") --> doCenterFloat
    , (className =? "firefoxdeveloperedition" <&&> role =? "Organizer") --> doCenterFloat
    , className =? "Float Term" --> doCenterFloat
    , className =? "Gimp" --> doFloat
    , className =? "jetbrains-idea" --> doCenterFloat
    , className =? "jetbrains-studio" --> doCenterFloat
    , resource =? "libreoffice" --> doShift (myWorkspaces !! 4)
    , className =? "mpv" --> doCenterFloat
    , className =? "qutebrowser" --> doShift (myWorkspaces !! 2)
    , className =? "scrcpy" --> doCenterFloat
    , className =? "SimpleScreenRecorder" --> doShift (myWorkspaces !! 8) <+> doCenterFloat
    , className =? "Spotify" --> doShift (myWorkspaces !! 5)
    , className =? "svkbd" --> doFloat
    , className =? "TelegramDesktop" --> doShift (myWorkspaces !! 6) <+> doCenterFloat
    , className =? "Tor Browser" --> doShift (myWorkspaces !! 2) <+> doCenterFloat
    , className =? "zoom" --> doCenterFloat
    , resource  =? "desktop_window" --> doIgnore
    ]
  where role = stringProperty "WM_WINDOW_ROLE"

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = ewmhDesktopsEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
bar::String
bar = "xmobar"

barConfig::PP
barConfig = xmobarPP {
      ppCurrent = xmobarColor "#7aa2f7" "" . xmobarBorder "Top" "#7aa2f7" 2
    , ppVisible = xmobarColor "#444b6a" ""
    , ppHidden = xmobarColor "#444b6a" ""
    , ppLayout = xmobarColor "#787c99" ""
    , ppTitleSanitize = xmobarStrip
    , ppTitle = xmobarColor "#a9b1d6" "" . shorten 40
    , ppWsSep = ""
    , ppSep = xmobarColor "#32344a" "" " • "
}

toggleBar XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

myLogHook = dynamicLogWithPP

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = xmonad $ defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = ewmhFullscreen
        . ewmh
        . withEasySB (statusBarProp "xmobar" (pure barConfig)) defToggleStrutsKey
        $ def {
      -- simple stuff
        terminal           = terminalCmd,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = superKey,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts myLayout,
        manageHook         = myManageHook <+> manageDocks,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'Super'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch st",
    "alt-Enter        Launch dmenu",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- Cycle between visible workspaces",
    "alt-Shift-}  Move to the next non empty workspace",
    "alt-Shift-{  Move to the previous non empty workspace",
    "-- Move focused window between workspaces and switch to that worksopace",
    "mod-Shift-}  Move focused window to the next workspace",
    "mod-Shift-{  Move focused window to the previous workspace",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1        Set the window to floating mode and move by dragging",
    "mod-button2        Raise the window to the top of the stack",
    "mod-button3        Set the window to floating mode and resize by dragging",
    "mod-button4        Move to the next non empty workspace",
    "mod-button5        Move to the previous non empty workspace",
    "mod-Shift-button4  Send focused window to the next workspace and switch to that worksopace",
    "mod-Shift-button5  Send focused window and the previous worksopace switch to that worksopace"]
