Development Setup
---

The package includes a `shell.nix` file to manage development dependencies. You can install nix
from [their website][install-nix]. You can enter the development environment with `nix-shell`,
or if using `direnv` by adding a `.envrc` file with the following,

```
use nix
```

Project Goals
---

Ultimate goal is [this website][html-to-elm] but for purescript w/ tailwind
support. Example use case, you have purchased the components from
[tailwindui][tailwind-ui] and want to use them in your Purescript project. The
project should be easily distributable in Purescript via npm. The tool should
be useable from the command line for things like CI/classname generation.

Available Tools
---

- Existing purescript syntax tree parsing [here][purescript-syntax-parser]
- Pretty purescript codegen tool [here][purescript-tidy-codegen]
- Existing solution without tailwind support [here][purescript-codegen-halogen]

TODO
---

- Library: common components used in CLI/Web
  - [ ] Barry: download the tailwind "manifest" and store it in local cache if needed
  - [ ] Christian: Rewrite the CSS parser or do bindings to an existing JS library
  - [ ] Barry: detection of existing tailwind classes in a project to generate Tailwind.purs and optimized CSS
- CLI
  - [ ] Barry: architect the CLI interface
- Web App

Future
---

- CSS flows (CLI)
- HTML flows
  - [ ] Reuse [this][purescript-codegen-halogen] to generate halogen code
    - Could this only be part of the web app?
    - Does this handle?
      - SVGs
      - ARIA
      - Tailwind
  - [ ]
- [ ] Ship a library with all the default tailwind classes (to avoid CSS flow entirely)

<!-- Links -->
[install-nix]: https://nixos.org/download.html
[html-to-elm]: https://html-to-elm.com
[tailwind-ui]: https://tailwindui.com
[purescript-syntax-parser]: https://github.com/natefaubion/purescript-language-cst-parser
[purescript-tidy-codegen]: https://github.com/natefaubion/purescript-tidy-codegen
[purescript-codegen-halogen]: https://github.com/ongyiren1994/purescript-html-codegen-halogen
