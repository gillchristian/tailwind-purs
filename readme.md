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

There are 3 commands.

- `gen-available-classes`: generates a file with the list of available Tailwind
  classes. This file is used as the input to the other commands. The input
  should be the CSS file with all the generated Tailwind styles.
- `gen-purs`: looks for all the `T.className` ocurrences and generates a
  PureScript file containing only those as Halogen `ClassName`s.To generate all
  the available ones instead use the `--all` flag. This command is ment both for
  development to generate all the available classes but also can be run to work
  with only the used ones, which reduces compile times a lot.
- `gen-css`: looks for all the `T.className` ocurrences and generates an
  optimized version of the `tailwind.css` file.

**gen-available-classes**

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

## LICENSE

[MIT License](https://github.com/gillchristian/tailwind-purs/blob/master/LICENSE) ©
[Christian Gill](https://gillchristian.xyz)
