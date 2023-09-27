return {
  {
    'lewis6991/impatient.nvim',
},
  {
  'mrcjkb/haskell-tools.nvim',
  dependencies = {
    'nvim-lua/plenary.nvim',
  },
  version = '^2', -- Recommended
  ft = { 'haskell', 'lhaskell', 'cabal', 'cabalproject' },
  config = function()

  hls = {
    -- for hls development
    cmd = { 'cabal', 'run', 'haskell-language-server' },
    on_attach = function(client, bufnr, ht)
      lsp.on_attach(client, bufnr)
      lsp.on_dap_attach(bufnr)
    end,
    capabilities = lsp.capabilities,
    default_settings = {
      haskell = {
        formattingProvider = 'stylish-haskell',
        maxCompletions = 10,
      },
    },
  }
      end
}

}
