# minesweeper #

This is an implementation of Minesweeper in Emacs.

There's a [bug tracker](https://todo.sr.ht/~zck/minesweeper).

### Installing minesweeper.el ###

There are two ways to install minesweeper:

1. **[MELPA](http://melpa.milkbox.net/)** -- a package manager for emacs. This is the recommended way to install minesweeper.el
    1. Follow the instructions under "installing" on MELPA's [getting started](http://melpa.milkbox.net/#/getting-started) page.
    2. Restart Emacs, to make it reload your init file.
    3. Run `M-x package-list-packages`.
    4. Find this package in the package list, and move point (the cursor) there.
    5. Press `i` to mark the package for installation.
    6. Press `x` to execute the commands and install the marked package.

2. You can install the game from source. This is not recommended unless you're changing the code.
    1. Clone this repository.
    2. Run `M-x load-file`, then navigate to the repository, and load `minesweeper.el`


### Playing ###
* The goal is to reveal all squares that do not have a mine. If you reveal a mine, the game is over. Play carefully!
* Point can be moved with the arrow keys, p/n/b/f, or C-p/C-n/C-b/C-f.
* To reveal squares, either left-click on them, or move point and press space, enter, or `x`.
* When a square is revealed, if it is not a mine, there will be a number indicating the count of mines in the neighboring eight squares.
* If a square with no neighboring mines is revealed, all its neighbors will also be revealed.
* Squares can be marked as having a mine in them. To do this, right-click on the square, or move point to it and press `m`. Marked squares are protected from being revealed by any means.
* You can choose to reveal all the neighbors of a square by middle-clicking on a square, or moving point there and pressing `c`.
