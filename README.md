# dotme

This is meant to be a portable environment system. My own shell commands, my favorite libraries, my favorite settings for my favorite languages, my own static www site(s), etc etc.

## Directory structure

How should this be structured? Ideally, I want all my system stuff to be separte from my code, from my website, etc, but I want them all to be able to call each other too.

```
~
...
|-.me
  |- system (for all of my environment/system setup)
     |- emacs
	 |- zsh
	 ...
  |- www (my static website at www.bsima.me)
  |- org (my private org files - sync'd via bittorrent/dropbox/rsync)
  |- maths (my maths files)
     |- euler (my project euler solutions)
  |- <commands>
...
```

`<commands>` represents different functions written in Common Lisp that are pre-loaded via [Shelly][1] and into every SBCL repl.

## Todo

- [ ] incorporate [dotfiles][2] into this
- [x] move www.bsima.me (currently at `~/sites/me/bsima`) into `~/.me/www`
- [ ] study Howard Abram's [dotfiles](https://github.com/howardabrams/dot-files) and implement parts of that

[1]: https://github.com/fukamachi/shelly
[2]: https://github.com/bsima/dotfiles
