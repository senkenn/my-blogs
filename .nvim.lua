vim.api.nvim_create_autocmd("BufWritePre", {
	pattern = "*.hs",
	callback = function()
		vim.cmd("LspDocumentFormat")
	end,
})
