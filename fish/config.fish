alias koka /home/david/GitRepos/koka/out/debug/koka-0.8.0-dev 
set EDITOR emacs
set PATH /opt/ghc/bin ~/.npm-global/bin ~/.cabal/bin /usr/lib/go-1.10/bin $PATH
set CHROME_BIN /usr/bin/chromium-browser

# OPAM configuration
source /home/david/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true

set fish_git_dirty_color red
set fish_git_not_dirty_color green

function parse_git_branch
    set -l branch (git branch 2> /dev/null | grep -e '\* ' | sed 's/^..\(.*\)/\1/')
    set -l git_status (git status -s)

    if test -n "$git_status"
	echo (set_color $fish_git_dirty_color)$branch(set_color normal)
    else
	echo (set_color $fish_git_not_dirty_color)$branch(set_color normal)
    end
end

    
