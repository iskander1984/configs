# Set up the prompt

HOSTNAME=icarus

autoload -Uz promptinit
promptinit
prompt walters

bindkey -e

# Set default editor
export EDITOR="emacs -nw"

export MPD_HOST=localhost
export MPD_PORT=6600

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Environmental variables

# add xmonad executable to $PATH
if [ -d ~/.xmonad ]; then
    PATH=$PATH:~/.xmonad
    export PATH
fi

if [ -d ~/bin ]; then
    PATH=~/bin:$PATH
    export PATH
fi

M2_HOME=/home/data/dev/bin/apache-maven-3.0.3
if [ -d $M2_HOME ]; then
    export M2_HOME
    export PATH=$M2_HOME/bin:$PATH
fi

GRAILS_HOME=/home/data/dev/bin/grails/grails-2.0.0.M1
if [ -d $GRAILS_HOME ]; then
    export GRAILS_HOME
    export PATH=$GRAILS_HOME/bin:$PATH
fi

GROOVY_HOME=/opt/groovy
if [ -d $GROOVY_HOME ]; then
    export GROOVY_HOME
    export PATH=$GROOVY_HOME/bin:$PATH
fi

ECLIPSE_DIR=/home/data/dev/bin/eclipse
if [ -d $ECLIPSE_DIR ]; then
    PATH=$ECLIPSE_DIR:$PATH
    export PATH
fi

JAVA_HOME=/opt/java
if [ -d $JAVA_HOME ]; then
    export JAVA_HOME
    PATH=$JAVA_HOME/bin:$PATH
fi

ANT_HOME=/home/data/dev/bin/apache-ant-1.8.2
if [ -d $ANT_HOME ]; then
    PATH=$ANT_HOME/bin:$PATH
    export ANT_HOME
fi

CLOJURE_HOME=/home/data/dev/bin/clojure/clojure-1.3.0
if [ -d $CLOJURE_HOME ]; then
    PATH=$CLOJURE_HOME/bin:$PATH
    export CLOJURE_HOME
fi

CLOJURE_LIB=/home/data/dev/bin/clojure/lib
if [ -d $CLOJURE_LIB ]; then
    export CLOJURE_LIB
fi

# aliases
alias cdmus='cd /home/data/music'
alias cdmov='cd /home/data/movies'
alias cddev='cd /home/data/dev/'
alias cdproj='cd /home/data/dev/projects'
alias cdlib='cd /home/data/library'
alias emacs='emacs -nw'
alias sqlplus='rlwrap sqlplus'
alias start-ora='sudo /etc/rc.d/oracle-xe start'
alias stop-ora='sudo /etc/rc.d/oracle-xe stop'
alias ls='ls --color'

# aliases for Git
alias ga='git add'
alias gp='git push'
alias gl='git log'
alias gs='git status'
alias gd='git diff'
alias gdc='git diff --cached'
alias gm='git commit -m'
alias gma='git commit -am'
alias gb='git branch'
alias gc='git checkout'
alias gra='git remote add'
alias grr='git remote rm'
alias gpu='git pull'
alias gcl='git clone'
