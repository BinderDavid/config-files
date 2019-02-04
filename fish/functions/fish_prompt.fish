function fish_prompt --description 'Write out the prompt'
    set -l last_status $status
    set -l git_dir (git rev-parse --git-dir 2> /dev/null)

    # User
    set_color $fish_color_user
    echo -n (whoami)
    set_color normal

    echo -n ': '

    if test -n "$git_dir"
	echo -n '['
	echo -n (parse_git_branch)
	echo -n '] '
    end
    
    # PWD
    set_color $fish_color_cwd
    echo -n (prompt_pwd)
    set_color normal

    #if not test $last_status -eq 0
    #  set_color $fish_color_error
    #end
    echo -n ' ➤ '
    set_color normal
end
