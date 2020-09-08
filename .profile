# Editor
export VISUAL="emacs"
export EDITOR="$VISUAL"
set -o emacs

# Wayland stuff
export GDK_BACKEND=wayland
export MOZ_ENABLE_WAYLAND=1

# Useful
export PAGER=less
export _JAVA_AWT_WM_NONREPARENTING=1
export JAVA_HOME=/usr/lib/jvm/openjdk11/
export LPASS_CLIPBOARD_COMMAND=/bin/wl-copy

# PATH
#export PATH="$HOME/.config/composer/vendor/bin:$HOME/bin:/usr/local/bin:/bin:/usr/bin:/usr/local/sbin:/usr/sbin:/sbin:$HOME/.cargo/bin:$HOME/go/bin:$HOME/bin/node-v14.6.0-linux-x64/bin:/opt/texlive/2020/bin/x86_64-linuxmusl:$HOME/.local/bin"

# Source .kshrc
. $HOME/.kshrc

# If running from tty1 start sway
if [ "$(tty)" = "/dev/tty1" ]; then
    exec dbus-run-session sway
fi
