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

-- plugins
cmd 'packadd paq-nvim'               -- load the package manager
local paq = require('paq-nvim').paq  -- a convenient alias
paq {'savq/paq-nvim', opt = true}    -- paq-nvim manages itself

paq {'mhartington/oceanic-next'}
paq {'nvim-treesitter/nvim-treesitter'}
paq {'neovim/nvim-lspconfig'}
paq {'glepnir/lspsaga.nvim'}
paq {'junegunn/fzf', run = fn['fzf#install']}
paq {'junegunn/fzf.vim'}
paq {'ojroques/nvim-lspfuzzy'}
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
paq {'romgrk/barbar.nvim'}
paq {'kyazdani42/nvim-tree.lua'}

require('gitsigns').setup()
require('rust-tools').setup({})
require('nvim-web-devicons').setup { default = true; }
require('lualine').setup()
require('nvim-tree').setup {}

-- options
cmd 'colorscheme OceanicNext'

opt.compatible = false
opt.autoread = true

opt.termguicolors = true
opt.mouse = 'a'
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

map('n', '<A-q>', ':qa<Enter>', keymap_opts)
map('n', '<Space>bb', ':Buffers<CR>', keymap_opts)
map('n', '<Space>ff', ':NvimTreeToggle<CR>', keymap_opts)

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
      ['<CR>'] = cmp.mapping.confirm({ select = true }),
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
    ensure_installed = 'maintained', highlight = {enable = true}
}

require('lspfuzzy').setup {}

local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  local opts = { noremap=true, silent=true }

  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
end

-- Call 'setup' on multiple servers and map buffer local keybindings 
-- when the language server attaches
local nvim_lsp = require('lspconfig')

local servers = { 'bashls', 'pyright', 'terraformls', 'rust_analyzer' }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    },
    capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
  }
end

-- tab bar
map('n', '<A-,>', ':BufferPrevious<CR>', keymap_opts)
map('n', '<A-.>', ':BufferNext<CR>', keymap_opts)
map('n', '<A-<>', ':BufferMovePrevious<CR>', keymap_opts)
map('n', '<A->>', ':BufferMoveNext<CR>', keymap_opts)
map('n', '<A-1>', ':BufferGoto 1<CR>', keymap_opts)
map('n', '<A-2>', ':BufferGoto 2<CR>', keymap_opts)
map('n', '<A-3>', ':BufferGoto 3<CR>', keymap_opts)
map('n', '<A-4>', ':BufferGoto 4<CR>', keymap_opts)
map('n', '<A-5>', ':BufferGoto 5<CR>', keymap_opts)
map('n', '<A-6>', ':BufferGoto 6<CR>', keymap_opts)
map('n', '<A-7>', ':BufferGoto 7<CR>', keymap_opts)
map('n', '<A-8>', ':BufferGoto 8<CR>', keymap_opts)
map('n', '<A-9>', ':BufferGoto 9<CR>', keymap_opts)
map('n', '<A-0>', ':BufferLast<CR>', keymap_opts)
map('n', '<A-c>', ':BufferClose<CR>', keymap_opts)
map('n', '<Space>bn', ':BufferOrderByBufferNumber<CR>', keymap_opts)
map('n', '<Space>bd', ':BufferOrderByDirectory<CR>', keymap_opts)
map('n', '<Space>bl', ':BufferOrderByLanguage<CR>', keymap_opts)

vim.g.bufferline = {
  animation = false,
  tabpages = true,
  closable = true,
  clickable = true,
  icons = numbers,
  insert_at_end = true,
}
