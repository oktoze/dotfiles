#!/bin/sh

setxkbmap us

i3lock -i ~/Pictures/lock.png

setxkbmap us,ir -option grp:ctrls_toggle
xmodmap ~/.Xmodmap
