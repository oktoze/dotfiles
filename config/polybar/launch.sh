#!/usr/bin/env bash

dir="$HOME/.config/polybar-config"

launch_bar() {
	# Terminate already running bar instances
    killall -q polybar

	# Wait until the processes have been shut down
    while pgrep -u $UID -x polybar > /dev/null; do sleep 1; done

	# Launch the bar
    if type "xrandr"; then
    for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
        MONITOR=$m polybar -q main -c "$dir/forest/config.ini" &
            done
		else
    polybar -q main -c "$dir/forest/config.ini" &
        fi
}


launch_bar
