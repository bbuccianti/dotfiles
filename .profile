# Editor
export VISUAL="emacs"
export EDITOR="$VISUAL"
set -o emacs

# Wayland stuff
#export GDK_BACKEND=wayland
#export MOZ_ENABLE_WAYLAND=1

# Useful
export PAGER=less
#export _JAVA_AWT_WM_NONREPARENTING=1
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk
#export LPASS_CLIPBOARD_COMMAND=/usr/bin/wl-copy
#export XDG_CURRENT_DESKTOP=sway

# PATH
export PATH="$HOME/bin:$PATH:/usr/sbin:/sbin:$HOME/.npm/bin:$HOME/go/bin"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# export LC_ALL=C.UTF-8
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib64:/usr/local/lib

# if test -z "${XDG_RUNTIME_DIR}"; then
#     export XDG_RUNTIME_DIR=/tmp/${UID}-runtime-dir
#     if ! test -d "${XDG_RUNTIME_DIR}"; then
#         mkdir "${XDG_RUNTIME_DIR}"
#         chmod 0700 "${XDG_RUNTIME_DIR}"
#     fi
# fi
