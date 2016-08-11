# pandoc-include
A Pandoc filter that replaces include labeled Code Blocks with the contents of
the referenced files. Even nested, recursive includes.

Based on the scripting tutorial for Pandoc:
http://pandoc.org/scripting.html#include-files

## Format
The Code Blocks like the following will include every file in a new line. The
reference paths should be either absolute or relative to the folder where the
pandoc command will be executed.

    ```include
    /absolute/file/path.md
    relative/to/the/command/root.md
    #do/not/include/this.md
    ```
If the file does not exist, it will be skipped completely. No warnings, no
residue, nothing. Putting an `#` as the first character in the line will make the
filter skip that file.

For now the nested includes only work for two levels, after that the source
will be inserted and not parsed.

*Note: the metadata from the included source files are discarded.*

Alternatively, use one of the following to increase all the header levels in the
included file. The first option is a shortcut for incrementing the level by 1.
The second demonstrates an increase of 2.

    ```include-indented

    ```{ .include header-change=2 }


If the header level is increased, the title from the included file is inserted at the
beginning of the included file as a header, at the level of the header level change. For
example, if the header is incremented by 1, the title is inserted as a level 1 heading.

## Installation
One could either install it using the Cabal packaging system by running:

```
cabal update
cabal install pandoc-include
```

Or it is also possible to use the pipe method using the source code described in the *Usage* section.

## Usage
In order to use this Pandoc filter, one has to include it into the Pandoc transformation workflow. This can be done by using the `--filter` parameter, like so:

```
pandoc --from markdown --to latex --filter pandoc-include input.md
```

All this does in the background is pipelining the output of Pandoc and the last filter into the standard input of the include filter and using its output as the next filter's input:

```
pandoc --from markdown --to json input.md | runhaskell IncludeFilter.hs | pandoc --from json --to latex
```

## License
Copyright ©2015 [Dániel Stein](https://twitter.com/steindani)

Source code is released under the Terms and Conditions of [MIT License](http://opensource.org/licenses/MIT).
