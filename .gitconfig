[user]
    name =
    email =
[color]
    branch = auto
    diff = auto
    interactive = auto
    status = auto
[core]
    autocrlf = false
    editor = emacsclient
    whitespace = fix,-indent-with-non-tab,trailing-space,cr-at-eol
    excludesfile = ~/.gitignore_global
    pager = delta --dark
[filter "lfs"]
    clean = git-lfs clean -- %f
    smudge = git-lfs smudge -- %f
    process = git-lfs filter-process
    required = true
[tag]
    sort = version:refname
