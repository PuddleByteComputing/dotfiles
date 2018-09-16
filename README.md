# PuddleByte dotfiles

Based on the [Dickson Labs dotfiles](https://github.com/dicksonlabs/dotfiles)

These are config files suitable for Rails/React development with emacs.

Provided: config files for emacs, the bash shell, rvm, irb, rails, and tmux

Outdated, unmaintained: configs for screen, vim, ack, ctags, and probably other stuff

## Installation

You will need `git` installed.
You should install `ruby` via [rvm](https://rvm.io/rvm/install)
Install `rake` with `gem install rake` if it's not provided with your ruby executable.

Then:
```
git clone https://github.com/PuddleByteComputing/dotfiles.git
cd dotfiles
rake
```

### Aliases

`p <dir>` will cd to your Projects directory

`gs` for `git status` (apologies to ghostscript users)
`ga` for `git add`
`gc` for `git commit`
`gd` for `git diff`
`gpu` for `git push origin`

`rdm` for `rake db:migrate`
`pdm` for `rake parallel:migrate` when using parallel tests
`pdb` to reinitialize all test dbs when using parallel tests

### Git

`git cleanup` will remove any branches that have been merged into the current branch

This generates a global ~/.gitconfig without credentials; you'll need to do the following per-project:

```
git config user.name = "Fred Flinstone"`
git config user.email = "fred@bedrock.example.com"
```

Some autocompletion is available, if you're not already using what's provided by your OS/package manager:

First symlink the git-completion script from the dotfiles repo:

    ln -s bash/completion_scripts/git_completion ~/.git-completion.bash

Second, add this to your `.localrc`

    if [ -f ~/.git-completion.bash ]; then
      . ~/.git-completion.bash
    fi

### Bash shell customization

You can override or add to the bash config provided in a `~/.localrc` file. This will
be loaded automatically if it exists.

### Emacs customization

You can add to the emacs config provided in a `~/.emacs_local.el` file, which will be
picked up automatically if it exists.

### Allow underlining in terminal emacs under tmux

Taken from [here](http://superuser.com/questions/529655/correct-way-to-get-emacs-16-color-support-inside-tmux).
build a custom terminfo entry that clears ncv with a command like this:

    { infocmp -x screen-256color; printf '\t%s\n' 'ncv@,'; } > /tmp/t && tic -x /tmp/t

(when run as non-root, it will write a new screen-256color entry under ~/.terminfo/)
