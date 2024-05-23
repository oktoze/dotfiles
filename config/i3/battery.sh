
#!/usr/bin/env sh

WARN_LEVEL=15;
CRITICAL_LEVEL=5;

warned=false;
critical_warned=false

while true
do
    battery_percentage=`upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep "percentage" | awk '{print substr($2, 1, length($2)-1)}'`
    state=`upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep "state:" | awk '{print $2}'`

    if [ $state == "discharging" ]
    then
        if [ $warned == false ] && [ $battery_percentage -le $WARN_LEVEL ]
        then
            notify-send -u critical -i $HOME/Pictures/lowbattery.png "Low battery, connect the charger!";
            warned=true;
        fi

        if [ $critical_warned == false ] &&  [ $battery_percentage -le $CRITICAL_LEVEL ]
        then
            notify-send -u critical -i $HOME/Pictures/lowbattery.png "Low battery, connect the charger!";
            critical_warned=true;
        fi
    fi
    sleep 30;
done
