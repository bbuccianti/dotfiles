# Prompt
export PS1='\e[35m[\A] \e[31m\u\e[32m:\e[34m\w\e[32m \$ \e[0m'

# History file
export HISTFILE="$HOME/.kshistory"
export HISTSIZE=10000
export HISTCONTROL="ignoredups:ignorespace"

# Functions
help() { curl "cht.sh/$1"; }
m() { doas mount "/mnt/atom/$1"; }
u() { doas umount "/mnt/atom/$1"; }

