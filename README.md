# modeling

## Development

Install `stack` and get rolling. On Mac you can just run `brew bundle` here.

	make build     # Build it (including test target)
	make test      # Run the tests
	make deps      # Install dev dependencies (linter, formatter, etc.)
	make watch     # Starts a ghcid server to do on the fly compilation
	make lint      # Run hlint but don't auto-fix errors
	make refactor  # Run hlint and auto-fix errors
	make format    # Reformat code with stylish-haskell
