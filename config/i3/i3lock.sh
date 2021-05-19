#!/bin/sh

SCREENSHOT_PATH=~/.tmp/i3lock.png
BLUR="0x"

scrot $SCREENSHOT_PATH --quality 100
convert $SCREENSHOT_PATH -scale 2.5% -resize 4000% $SCREENSHOT_PATH
i3lock -i $SCREENSHOT_PATH
rm $SCREENSHOT_PATH
