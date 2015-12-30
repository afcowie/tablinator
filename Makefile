all: experiment

experiment:
	stack build

tags:
	codex update

pdf:
	pandoc -o junk.pdf examples/junk.markdown
