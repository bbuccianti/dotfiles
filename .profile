# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

export PATH="$PATH:/usr/sbin"

# Editor
export VISUAL="emacs"
export EDITOR="$VISUAL"
set -o emacs

# Useful
export PAGER=less

export DISPLAY=$(awk '/nameserver/ {print $2; exit;}' /etc/resolv.conf):0

# PROXY
#export HTTP_PROXY=http://192.168.0.99:8080/
#export HTTPS_PROXY=http://192.168.0.99:8080/
