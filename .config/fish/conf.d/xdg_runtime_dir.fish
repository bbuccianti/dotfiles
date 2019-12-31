if test -z $XDG_RUNTIME_DIR
  set -x XDG_RUNTIME_DIR /tmp/(id -u)-runtime-dir
  if not test -d $XDG_RUNTIME_DIR
    mkdir $XDG_RUNTIME_DIR
    chmod 0700 $XDG_RUNTIME_DIR
    exec sway
  end
end