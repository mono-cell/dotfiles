return {
  "norcalli/nvim-colorizer.lua",
    lazy = false,
  priority = 1000,
  config = function()
    require("colorizer").setup()
  end,
  cmd = {
    "ColorizerAttachToBuffer",
    "ColorizerDetachFromBuffer",
    "ColorizerReloadAllBuffers",
    "ColorizerToggle",
  },
}
