local parser_config = require"nvim-treesitter.parsers".get_parser_configs()
parser_config.nu = {
    install_info = {
        url = "~/Documents/personal/experiments/tree-sitter-nu", -- local path or git repo
        files = {"src/parser.c"}
    }
}
