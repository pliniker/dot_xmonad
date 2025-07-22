require("nvchad.configs.lspconfig").defaults()

local servers = { "basedpyright", "ruff", "marksman" }
vim.lsp.enable(servers)

-- read :h vim.lsp.config for changing options of lsp servers 
vim.diagnostic.config({
  virtual_text = false
})

-- Show line diagnostics automatically in hover window
vim.diagnostic.config({ virtual_text = false, virtual_lines = { current_line = true }, })
