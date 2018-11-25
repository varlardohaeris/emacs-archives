mazemax.el lets you to play around with mazes in an Emacs buffer.

You can either use a preexisting buffer and turn on
mazemax-ra-minor-mode in it or run M-x mazemax to generate a random
maze and play with it immediately.

Here is a sample maze.

To test the minor mode you can move the cursor inside
this little maze and activate 'mazemax-ra-mode': further cursor
motion is limited to free (generally white space) cells.

888888888888888888888888888888888888888888888888888888888888
8                      8    8               8          8   8
8 8888888888  8888888 88 8888 88 8 8 8 8  888 88 8  88 8 888
8  8    8    88 8      8 8 8   8 8 888888  8   8888  8   8 8
8 88888   8 88     8 8   8 88 88 8 8   8  88 8 8    8888 8 8
8 8   88888  8 8 8 8 8 8   8  8  88888       8 888888      8
8 8 8    8  88 8 88888888  8888888     88  8 8 8      8 88 8
8 8 888     8  8 8        88  8  88888 8  888888888888888  8
8 8 8   8 8 88888888 8888       88     88  8   8           8
888888 888888     8  8    88 8   88888 8  8888 888888888 888
8  8   8  8 88888 8888888  8 8 8 8 8 8 88 8 8  8         8 8
8 8888888 8 8           88 8 8 8   8    888       8 8 8    8
8   8 8   8 88888888888  8 888888888888   8 8 8 8 8 888888 8
8  88 88              88 8   8     8 8   8888 88888 8      8
8 88  8  8 8  8 88 88  8   888 88888 8 8    888   88888888 8
8 8   8 88888 8  8 8  8888       8 8 888 8    8 8   8 8    8
8 8 8    8  8 8888888  8 8 8888888 8     8 8  8 8 8 8 8888 8
8 888888   88 8 8  8  88 8     8 8 8  88888888888 8    8 8 8
8 8  8 8 8  8 8       8    8 8   8 88 8   8 8   8 8 8 88   8
8 88 8 8 8 88 888 888888 8888888 8 8  8 888 88 8888 8  888 8
8        8  8  8           8                   8    8      8
888888888888888888888888888888888888888888888888888888888888

The generated mazes, like the one above, are guaranteed to
solvable.  This means that any free cell of the maze should be
reachable from any other.

If you find the generated mazes too easy, try the interactive
command mazemax-large.  This will create a maze larger than the
buffer window (generation may take some time on some systems, but
byte compilation helps).

Faces and characters used by the generator are customizable, via
M-x customize-group mazemax.

Implementation notes:

The default maze generator algorithm employed, mazemax-parallel, is
designed to have roughly O(n*log(sqrt(n))) time complexity (where n
is the area of the maze to be generated).  During maze generation,
a list of candidate growing points is updated until all valid
growing points are exhausted (growing points are considered valid
as long as they do not violate the maze solubility rule).

For simplicity, none of the minor modes have an indicator in the
mode-line.
