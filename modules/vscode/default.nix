{ pkgs, ... }: {
  programs.vscode = {
    enable = true;
    extensions = with pkgs.vscode-extensions; [
      ms-python.python
      ms-python.vscode-pylance
      charliermarsh.ruff
      matangover.mypy
      vscodevim.vim
    ];
  };
}
