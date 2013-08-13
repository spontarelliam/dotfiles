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
#export PAGER="vimpager"
#export PATH="${PATH}:${HOME}/bin:${HOME}/.cabal/bin"

#-----------------------------
# Dircolors
#-----------------------------
#LS_COLORS='rs=0:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:';
#export LS_COLORS

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

export GIT_AUTHOR_NAME="Adam S"
export GIT_COMMITTER_NAME="Adam S"

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/smed/Downloads/freesteam-2.0
export LD_LIBRARY_PATH

# Fix Firefox font rendering by disabling PANGO
export MOZ_DISABLE_PANGO=1

# Bind Keys
bindkey "^[[7~" beginning-of-line
bindkey "^[[8~" end-of-line
bindkey "\e[3~" delete-char

# Enable numlock on startup. Effective after login.
# causes error on some machines
#setleds -D +num

#------------------
# GERMAN SERVER
#------------------
if [[ $HOSTNAME = "spselc3a" ]]; then 
   echo "Working on " $HOSTNAME
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

export PATH=$PATH:/usr/local/sbin:/usr/sbin:/sbin
source /opt/intel/bin/compilervars.sh intel64
alias ifort='/opt/intel/composer_xe_2013.5.192/bin/intel64/ifort'
alias idbc='/opt/intel/composer_xe_2013.5.192/bin/intel64/idbc'
