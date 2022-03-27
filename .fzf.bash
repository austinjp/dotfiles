# Setup fzf
# ---------
if [[ ! "$PATH" == */home/austinjp/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/austinjp/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/austinjp/.fzf/shell/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/home/austinjp/.fzf/shell/key-bindings.bash"
