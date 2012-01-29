import XMonad
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Named
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.SimpleFloat
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Themes
import Data.Monoid
import Data.Ratio ((%))
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

------------------------------------------------------------------------

myTerminal      = "urxvtc"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth   = 1

myModMask       = mod4Mask

myWorkspaces    = ["1:web","2:dev","3:chat","4:irc","5:media","0:*"]

myNormalBorderColor  = "#5A5A5A"
myFocusedBorderColor = "#FF7700"

------------------------------------------------------------------------
-- Key bindings

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_run -nb Black -nf Gray -sb Black`")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    , ((modm, xK_backslash), withFocused (sendMessage . maximizeRestore))

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    --, ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Lock screen
    , ((modm .|. shiftMask  , xK_z  ), spawn "sflock")

    -- Start galculator
    , ((modm                , xK_c  ), spawn "galculator")

    -- Move to next empty workspace
    , ((modm                , xK_m  ), viewEmptyWorkspace)

    -- Move focused window to next empty workspace and view
    , ((modm .|. shiftMask  , xK_m  ), tagToEmptyWorkspace)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Multimedia keys
    --
    -- Volume Up
    , ((0, xF86XK_AudioRaiseVolume  ), spawn "amixer sset Master 5%+")
    -- Volume Down
    , ((0, xF86XK_AudioLowerVolume  ), spawn "amixer sset Master 5%-")
    -- Mute On/Off
    , ((0, xF86XK_AudioMute         ), spawn "amixer sset Master toggle")
    -- Play/Pause
    , ((0, xF86XK_AudioPlay         ), spawn "gmusicbrowser -launch_or_cmd PlayPause")
    -- Next
    , ((0, xF86XK_AudioNext         ), spawn "gmusicbrowser -remotecmd NextSong")
    -- Previous
    , ((0, xF86XK_AudioPrev         ), spawn "gmusicbrowser -remotecmd PrevSong")
    -- Stop
    , ((0, xF86XK_AudioStop         ), spawn "gmusicbrowser -remotecmd Stop")
    -- Calculator
    , ((0, xF86XK_Calculator        ), spawn "galculator")
    , ((0, xF86XK_Documents         ), spawn "beagle-search")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
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
-- Mouse bindings

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

basicLayout = Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2

wideLayout = smartBorders(  named "wide"    $ Mirror basicLayout)
tallLayout = smartBorders(  named "tall"    $ basicLayout)
tabbedLayout = noBorders(   named "fulltab" $ simpleTabbed)
floatLayout   = maximize (  named "float"   $ simpleFloat)
fullNoBorders = noBorders ( named "Full"    $ Full)
imLayout      = noBorders(  named "im"      $ reflectHoriz $ gridIM (1%5) (Role "buddy_list"))

myLayout = avoidStruts $
         onWorkspaces ["3:chat"] imLayout $
         fullNoBorders ||| basicLayout  ||| floatLayout

myManageHook = composeAll
    [
      className =? "Gimp"                        --> doFloat
    , className =? "Xmessage"                    --> doFloat
    , className =? "Select Font"                 --> doFloat
    , className =? "gtk-chtheme"                 --> doFloat
    , className =? "Google-chrome"               --> doShift "1:web"
    , className =? "Transmission"                --> doShift "1:web"
    , className =? "SpringSource Tool Suite"     --> doFloat
    , className =? "Eclipse"                     --> doFloat
    , className =? "Emacs"                       --> doShift "2:dev"
    , className =? "Xchat"                       --> doShift "3:chat"
    , className =? "Pidgin"                      --> doShift "3:chat"
    , className =? "Skype"                       --> doShift "3:chat"
    , className =? "Gnome-mplayer"               --> doShift "5:media"
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , manageDocks
    ]

------------------------------------------------------------------------
-- Event handling

myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

myLogHook = dynamicLogWithPP $ byorgeyPP

------------------------------------------------------------------------
-- My StatusBar

myBar = "xmobar /home/sober/.xmobarrc"
myPP = xmobarPP {
    ppCurrent = xmobarColor "#FFFFFF" "" . wrap "[" "]",
    ppUrgent                = xmobarColor "#FFFFFF" "#FF0000" . wrap "" "*" . xmobarStrip,
    ppTitle                 = xmobarColor "#FF7700" "" . shorten 50,
    ppSep                   = " | "
    }
toggleStrutsKey XConfig {XMonad.modMask = myMmodMask} = (myModMask, xK_b)

------------------------------------------------------------------------
-- Startup hook

myStartupHook = return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

main = xmonad =<< statusBar myBar myPP toggleStrutsKey (withUrgencyHook NoUrgencyHook $ myConfig)

myConfig = defaultConfig {

    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    logHook            = myLogHook,
    startupHook        = myStartupHook
}
