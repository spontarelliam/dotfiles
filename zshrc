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
export EDITOR="emacs"
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

alias ls='ls --color=auto'

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

# Load infinality font rendering
#. ~/path/to/this/file/infinality-settings.sh
setfont /usr/share/local/

# Fix Firefox font rendering by disabling PANGO
export MOZ_DISABLE_PANGO=1

# Bind Keys
bindkey "^[[7~" beginning-of-line
bindkey "^[[8~" end-of-line
bindkey "\e[3~" delete-char

# Ensure non-window emacs mode
#alias emacs='emacs -nw'

# SERVER
#------------------
autoload -U compinit promptinit
compinit
promptinit
 
# This will set the default prompt to the walters theme
prompt redhat

alias ls='ls --color=auto'

export TERM=xterm-256color

if [[ -n ${INSIDE_EMACS} ]]; then
    # This shell runs inside an Emacs *shell*/*term* buffer.
    prompt walters
    unsetopt zle
fi

# Enable numlock on startup. Effective after login.
setleds -D +num

