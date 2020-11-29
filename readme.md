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

There are 3 commands:

**gen**

... TODO ...

**purs**

```
$ twpurs purs --help
Usage: twpurs purs [ROOT] [--src SRC] [--classes CLASSES] [-o|--out OUT]
  Generate Tailwind.purs with only the used classes

Available options:
  ROOT                     Root directory of the project. The other paths are
                           relative this path. (default: ".")
  --src SRC                The source directory (default: "src")
  --classes CLASSES        File containing the available
                           classes (default: "css.txt")
  -o,--out OUT             Output file (default: "src/Tailwind.purs")
  -h,--help                Show this help text
```

**css**

```
$ twpurs css --help
Usage: twpurs css [ROOT] [--src SRC] [--classes CLASSES] [--css CSS]
                  [-o|--out OUT]
  Generate tailwind.css with only the used styles

Available options:
  ROOT                     Root directory of the project (default: ".")
  --src SRC                The PureScript source directory (default: "src")
  --classes CLASSES        File containing the available
                           classes (default: "css.txt")
  --css CSS                The file with all the generated Tailwind
                           CSS (default: "wip/tailwind.css")
  -o,--out OUT             Output file (default: "dist/tailwind.css")
  -h,--help                Show this help text
```
