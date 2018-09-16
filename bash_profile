# -*- sh -*-

# Read by login shells only (the first time you log in).
# NOT read by subshells (like what screen creates).

# Load the default .profile, if it exists
[[ -s "$HOME/.profile" ]] && source "$HOME/.profile"

# .bashrc is read by subshells. This will make login shells read it too.
[[ -s "$HOME/.bashrc" ]] && source "$HOME/.bashrc"

# load path fixer-upper after loading all other config files, BEFORE setting up rvm
source $HOME/.bash/better-paths

# get local ./node_modules/bin into PATH (similar to rvm)
#__OLD_PATH=$PATH
#function updatePATHForNPM() {
#  export PATH=$(npm bin):$__OLD_PATH
#}

#function node-mode() {
#  PROMPT_COMMAND=updatePATHForNPM
#}

#function node-mode-off() {
#  unset PROMPT_COMMAND
#  PATH=$__OLD_PATH
#}

# Uncomment to enable node-mode by default:
# node-mode

# Load RVM as a function
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# note: dedupe_path is defined in better-paths, loaded above
dedupe_path

# fix rvm problems caused by tmux
if [ -n $TMUX ] && [ 'function' = `type -t rvm` ]; then
    # NOTE: rvm does some hanky-panky with STDERR so it can always shout
    #   its warnings at you, even when you're running the command to fix the
    #   thing it's warning you about.
    # We redirect file descriptor 6 here to circumvent this nonsense.
    rvm use default >/dev/null 2>&1 6>&1
    cd .
fi
