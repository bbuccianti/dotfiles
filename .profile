# Editor
export VISUAL="emacs"
export EDITOR="$VISUAL"
set -o emacs

# Wayland stuff
#export GDK_BACKEND=wayland
export MOZ_ENABLE_WAYLAND=1

# Useful
export PAGER=less
export _JAVA_AWT_WM_NONREPARENTING=1
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk
export LPASS_CLIPBOARD_COMMAND=/usr/bin/wl-copy

# PATH
export PATH="$PATH:/sbin:$HOME/bin:$HOME/.npm/bin"

# If running from tty1 start sway
if [ "$(tty)" = "/dev/tty1" ]; then
    #exec dbus-run-session sway
    exec sway
fi
