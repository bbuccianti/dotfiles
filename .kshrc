# Prompt
PS1='\e[35m[\A] \e[31m\u\e[32m:\e[34m\w\e[32m \$ \e[0m'

# History file
HISTFILE="$HOME/.kshistory"
HISTSIZE=10000

# Functions
help() { curl "cht.sh/$1"; }
m() { doas mount "/mnt/atom/$1"; }
u() { doas umount "/mnt/atom/$1"; }

