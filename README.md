Development Setup
---

The package includes a `shell.nix` file to manage development dependencies. You can install nix
from [their website][install-nix]. You can enter the development environment with `nix-shell`,
or if using `direnv` by adding a `.envrc` file with the following,

```
use nix
```

[install-nix]: https://nixos.org/download.html
