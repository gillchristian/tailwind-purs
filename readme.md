# tailwind-purs

A CLI to optimize the generation of type safe Tailwind classes in PureScript

### Install

At the moment the only way to install is building from source:

```
$ git clone https://github.com/gillchristian/tailwind-purs.git
$ cd tailwind-purs
$ stack install .
```

This will install the `twpurs` command.

### Usage

**gen-available-classes**

Generates a file with the list of available Tailwind classes. This file is used
as the input to the other commands. The input should be the CSS file with all
the generated Tailwind styles.

```
$ twpurs gen-available-classes --help
Usage: twpurs gen-available-classes [ROOT] [--css CSS] [-o|--out OUT]
  Generate the list of available classes

Available options:
  ROOT                     Root directory of the project (default: ".")
  --css CSS                The file with all the Tailwind generated
                           CSS (default: "dev/tailwind.css")
  -o,--out OUT             Output file (default: "tailwind-classes.txt")
  -h,--help                Show this help text
```

**gen-purs**

Looks for all the `T.className` ocurrences and generates a  PureScript file
containing only those as Halogen `ClassName`s.To generate all the available ones
instead use the `--all` flag. This command is ment both for development to
generate all the available classes but also can be run to work with only the
used ones, which reduces compile times a lot.

```
$ twpurs gen-purs --help
Usage: twpurs gen-purs [ROOT] [--src SRC] [--classes CLASSES] [-o|--out OUT]
                       [--all]
  Generate Tailwind.purs with only the used classes

Available options:
  ROOT                     Root directory of the project. The other paths are
                           relative this path. (default: ".")
  --src SRC                The source directory (default: "src")
  --classes CLASSES        File containing the available classes. This is the
                           output from `$ twpurs gen-available-classes`
                           (default: "tailwind-classes.txt")
  -o,--out OUT             Output file (default: "src/Tailwind.purs")
  --all                    Generate all the available classes
  -h,--help                Show this help text
```

**gen-css**

Looks for all the `T.className` ocurrences and generates an optimized version of
the `tailwind.css` file.

```
$ twpurs gen-css --help
Usage: twpurs gen-css [ROOT] [--src SRC] [--classes CLASSES] [--css CSS]
                      [-o|--out OUT]
  Generate tailwind.css with only the used styles

Available options:
  ROOT                     Root directory of the project (default: ".")
  --src SRC                The directory with the PureScript
                           source (default: "src")
  --classes CLASSES        File containing the available classes. This is the
                           output from `$ twpurs gen-available-classes`
                           (default: "tailwind-classes.txt")
  --css CSS                The file with all the Tailwind generated
                           CSS (default: "wip/tailwind.css")
  -o,--out OUT             Output file. This should be the production CSS
                           file (default: "dist/tailwind.css")
  -h,--help                Show this help text
```

**html2purs**

Parse HTML and produce the Halogen HTML version, takes care of formatting the
classes as Tailwind ones. It does not pretty format the generated PureScript
code. You can use [purty](https://gitlab.com/joneshf/purty) for that.

```
$ twpurs html2purs --help
Usage: twpurs html2purs 
  Pars HTML (from STDIN) into Halogen HTML

Available options:
  -h,--help                Show this help text
```

Here's a bash function I use to do the formatting with `purty`.It expects the
HTML to be on the clipboard. Purty only formats valid PureScript modules, so the
function takes care of that. 

```bash
function html2purs {
  # On Mac use `pbpaste` instead
  # local out="$(pbpaste | twpurs html2purs | awk '{print "  " $0}')"
  local out="$(xclip -o -sel clip | twpurs html2purs | awk '{print "  " $0}')"
  echo "module Temp where\n\ntmp =\n$out" |  purty - | tail -n +4 | sed 's/^  //'
}
```

**classnames**

Transform a list of Tailwind classes into the PureScript version.

```
$ twpurs classnames --help
Usage: twpurs classnames [class] [-s|--single-line]
  Parse a list of CSS classes (from the HTML class attribute: `<div class="foo
  bar baz">`) and produce the Halogen PureScript version `[ T.foo, T.bar, T.baz
  ]`

Available options:
  -s,--single-line         Output in single line
  -h,--help                Show this help text
```

## LICENSE

[MIT License](https://github.com/gillchristian/tailwind-purs/blob/master/LICENSE) ©
[Christian Gill](https://gillchristian.xyz)
