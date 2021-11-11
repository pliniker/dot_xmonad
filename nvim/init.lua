--
-- # Setup
--
-- ## This file
--
-- Install into `$HOME/.config/nvim/init.lua`
--
-- ## Install paq
--
-- ```
-- git clone https://github.com/savq/paq-nvim.git \
--    "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/pack/paqs/opt/paq-nvim
-- ```
--
-- ## Then install plugins
--
-- ```
-- $ nvim
-- :PaqInstall
-- ```
--
-- ## Installing LSP servers
--
-- ### Python
--
-- ```
-- NPM_CONFIG_PREFIX=~/.npm/modules npm install -g pyright
-- ```
--
-- ### Terraform
--
-- Download Hashicorp terraform-ls, extract and place on $PATH
--
-- ### Rust
--
-- Download rust-analyzer and place on $PATH
--
-- ### Bash
--
-- ```
-- NPM_CONFIG_PREFIX=~/.npm/modules npm install -g bash-language-server
-- ```
--

-- shortcuts
local cmd = vim.cmd  -- to execute Vim commands e.g. cmd('pwd')
local fn = vim.fn    -- to call Vim functions e.g. fn.bufnr()
local g = vim.g      -- a table to access global variables
local opt = vim.opt  -- to set options
local map = vim.api.nvim_set_keymap

g.mapleader = " "

-- plugins
cmd 'packadd paq-nvim'               -- load the package manager
local paq = require('paq-nvim').paq  -- a convenient alias
paq {'savq/paq-nvim', opt = true}    -- paq-nvim manages itself

paq {'mhartington/oceanic-next'}
paq {'nvim-treesitter/nvim-treesitter'}
paq {'neovim/nvim-lspconfig'}
paq {'junegunn/fzf', run = fn['fzf#install']}
paq {'hrsh7th/cmp-nvim-lsp'}
paq {'hrsh7th/cmp-buffer'}
paq {'hrsh7th/cmp-vsnip'}
paq {'hrsh7th/vim-vsnip'}
paq {'hrsh7th/nvim-cmp'}
paq {'nvim-lua/plenary.nvim'}
paq {'lewis6991/gitsigns.nvim'}
paq {'simrat39/rust-tools.nvim'}
paq {'kyazdani42/nvim-web-devicons'}
paq {'hoob3rt/lualine.nvim'}
paq {'nvim-telescope/telescope.nvim'}
paq {'cappyzawa/trim.nvim'}
paq {'folke/which-key.nvim'}

require('nvim-web-devicons').setup { default = true; }
require('gitsigns').setup()
require('lualine').setup()
require('rust-tools').setup {}

require('telescope').setup {
  pickers = {
    buffers = {
      sort_mru = true,
      ignore_current_buffer = true,
      previewer = false
    }
  }
}

require('trim').setup {
  disable = {},
  patterns = {
    [[%s/\s\+$//e]]
  },
}

local wk = require("which-key")
wk.setup {}
wk.register(
  {
    f = {name = "files"},
    b = {name = "buffers"},
    g = {name = "git"},
    c = {name = "lsp-code"},
    w = {name = "lsp-workspace"},
  },
  { prefix = "<leader>" }
)

-- options
cmd 'colorscheme OceanicNext'

opt.compatible = false
opt.autoread = true

opt.termguicolors = true
opt.mouse = 'nv'
opt.number = true
opt.relativenumber = true
opt.ruler = true

opt.completeopt = {'menuone', 'noinsert', 'noselect'}
opt.hidden = true

opt.hlsearch = true
opt.incsearch = true
opt.ignorecase = true
opt.smartcase = true
opt.showmatch = true

opt.scrolloff = 4
opt.sidescrolloff = 8

opt.wildmode = {'list', 'longest'}
opt.wrap = false

opt.tabstop = 4
opt.shiftround = true
opt.shiftwidth = 4
opt.expandtab = true
opt.smartindent = true
opt.smarttab = true

opt.swapfile = false
opt.backup = false
opt.encoding = 'utf8'
opt.visualbell = false
opt.errorbells = false

local keymap_opts = { noremap = true, silent = true }

map('i', 'jj', '<Esc>', keymap_opts)
map('n', '<A-q>', ':qa<Enter>', keymap_opts)
map('i', '<C-H>', '<C-W>', keymap_opts)

map('n', '<leader>bb', ':Telescope buffers<CR>', keymap_opts)
map('n', '<leader>bd', ':bdelete<CR>', keymap_opts)

map('n', '<leader>fb', ':Telescope file_browser<CR>', keymap_opts)
map('n', '<leader>ff', ':Telescope git_files<CR>', keymap_opts)
map('n', '<leader>fg', ':Telescope live_grep<CR>', keymap_opts)

map('n', '<leader>gb', ':Telescope git_branches<CR>', keymap_opts)
map('n', '<leader>gc', ':Telescope git_commits<CR>', keymap_opts)
map('n', '<leader>gs', ':Telescope git_status<CR>', keymap_opts)

-- completion
local cmp = require('cmp')

cmp.setup({
  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  mapping = {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<Tab>'] = cmp.mapping.confirm({ select = true }),
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'vsnip' },
    { name = 'buffer' },
  }
})

-- lsp
require('nvim-treesitter.configs').setup {
  ensure_installed = 'maintained',
  highlight = { enable = true }
}

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap=true, silent=true }

  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<leader>cs', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<leader>cd', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<leader>ce', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', '<leader>cr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<leader>cf', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
  buf_set_keymap('n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<leader>we', ':Telescope lsp_workspace_diagnostics<CR>', opts)
  buf_set_keymap('n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
end

-- Call 'setup' on multiple servers and map buffer local keybindings
-- when the language server attaches
local nvim_lsp = require('lspconfig')

local servers = { 'bashls', 'pyright', 'terraformls', 'rust_analyzer' }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 500,
    },
    capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
  }
end

-- formatting on save
vim.api.nvim_command[[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()]]
