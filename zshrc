#!/bin/zsh
#------------------------------
# Comp stuff
#------------------------------
autoload -U compinit promptinit
compinit
promptinit
setopt correct

#------------------------------
# History stuff
#------------------------------
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

#------------------------------
# Variables
#------------------------------
export BROWSER="firefox"
export EDITOR="emacsclient"

#------------------------------
# Prompt
#------------------------------
autoload -U colors && colors
prompt suse

# use ls --color if not using a dumb terminal. Meant for emacs
if [ "$TERM" != "dumb" ]; then
  alias ls='ls --color=auto'
fi

# Arch message of the day
if [ -d "$HOME/Documents/archmotd" ]; then
    cat $HOME/Documents/archmotd
fi

if [ -d "$HOME/OpenFOAM/OpenFOAM-2.1.1/etc" ]; then
    source $HOME/OpenFOAM/OpenFOAM-2.1.1/etc/bashrc
fi

# Background
feh --bg-scale /usr/share/archlinux/wallpaper/archlinux-simplyblack.png

export GIT_AUTHOR_NAME="Adam S"
export GIT_COMMITTER_NAME="Adam S"

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/smed/Downloads/freesteam-2.0:/usr/local/lib
export LD_LIBRARY_PATH

# Fix Firefox font rendering by disabling PANGO
export MOZ_DISABLE_PANGO=1

alias fritzing='$HOME/Downloads/fritzing-0.8.0b.linux.AMD64/Fritzing'

# Bind Keys
bindkey "^[[7~" beginning-of-line
bindkey "^[[8~" end-of-line
bindkey "\e[3~" delete-char
setxkbmap -option ctrl:nocaps  # Make Caps Lock a Control key

# Enable numlock on startup. Effective after login.
# causes error on some machines
#setleds -D +num

#------------------
# GERMAN SERVER
#------------------
if [[ $HOSTNAME = "spselc3a" ]]; then 
   # Ensure non-window emacs mode
   alias emacs='emacs24.3 -nw'
   alias emacsold='/usr/bin/emacs -nw'

   export TERM=xterm-256color

   export TZ="/usr/share/zoneinfo/America/New_York"

   alias tree='~/Downloads/tree/bin/tree --charset=ASCII'

   if [[ -n ${INSIDE_EMACS} ]]; then
       # This shell runs inside an Emacs *shell*/*term* buffer.
       prompt walters
       unsetopt zle
   fi	
fi

source /opt/OpenFOAM/OpenFOAM-2.3.1/etc/bashrc

export PATH=$PATH:~/Downloads/git-annex.linux:/home/adam/Downloads/git-fat-master

export GIT_HTTP_USER_AGENT="Mozilla/5.0 (X11; Linux x86_64; rv:17.0) Gecko/20100101 Firefox/17.0"
# git config --global http.proxy $HTTP_PROXY

if [ -d "/opt/intel" ]; then
    export PATH=$PATH:/usr/local/sbin:/usr/sbin:/sbin
    source /opt/intel/bin/compilervars.sh intel64
    alias ifort='/opt/intel/bin/ifort'
    alias idbc='/opt/intel/bin/idbc'
fi

# added by Anaconda 1.8.0 installer
#export PATH="/opt/anaconda/bin:$PATH"

# eval $(ssh-agent)
# ssh-add

# Set okular as default pdf viewer
xdg-mime default okularApplication_pdf.desktop application/pdf 

# enable sysrq commands such as: Alt+SysRq+e
#sysctl kernel.sysrq=1

alias pandoc=~/.cabal/bin/pandoc

