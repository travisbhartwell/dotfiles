# My Dot Files

My configuration files are managed by the awesome [chezmoi](https://www.chezmoi.io/) tool.

This configuration currently supports:
- Mac OS on Apple Silicon
- Linux (specifically Raspberry Pi OS and Ubuntu on WSL)

## Bootstrapping a New Machine

``` shell
bootstrap_script="bootstrap-$(uname -r).sh" &&
    curl -sSL -O "https://raw.githubusercontent.com/travisbhartwell/dotfiles/main/bootstrap/${bootstrap_script}" &&
    chmod +x "${bootstrap_script}" &&
    ./"${bootstrap_script}" &&
    unset bootstrap_script
```
