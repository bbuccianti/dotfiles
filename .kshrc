# Emacs tramp!
[[ $TERM == "dumb" ]] && unset zle && PS1='$ ' && return

# Prompt!
export PS1=$'\1\e[31m\1$(whoami)\1\e[32m\1:\1\e[34m\1$(echo $PWD | sed "s,^$HOME,~,")\1\e[32m\1 $ \e[0m'

# History file
export HISTFILE="$HOME/.mkshistory"
export HISTSIZE=10000
export HISTCONTROL="ignoredups:ignorespace"

# Alias!
alias -x apku='doas apk -U upgrade'

# fzf keybindings
. $HOME/src/fzf-mksh/key-bindings.mksh

# Functions
help() { curl "cht.sh/$1"; }
m() { doas mount "/mnt/atom/$1"; }
u() { doas umount "/mnt/atom/$1"; }
