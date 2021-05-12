export SCRIPT_PATH=$(pwd)

rm ~/.zshrc
ln -s $SCRIPT_PATH/zshrc ~/.zshrc

rm ~/.Xmodmap
ln -s $SCRIPT_PATH/Xmodmap ~/.Xmodmap

rm -r ~/.doom.d/
ln -s $SCRIPT_PATH/doom.d ~/.doom.d

rm ~/.config/libinput-gestures.conf
ln -s $SCRIPT_PATH/config/libinput-gestures.conf ~/.config/libinput-gestures.conf

rm ~/.config/picom.conf
ln -s $SCRIPT_PATH/config/picom.conf ~/.config/picom.conf

rm -r ~/.config/nvim
ln -s $SCRIPT_PATH/config/nvim ~/.config/nvim

rm -r ~/.config/i3
ln -s $SCRIPT_PATH/config/i3 ~/.config/i3

rm -r ~/.config/polybar
ln -s $SCRIPT_PATH/config/polybar ~/.config/i3

rm -r ~/.config/dunst
ln -s $SCRIPT_PATH/config/dunst ~/.config/dunst
