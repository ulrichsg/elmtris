TARGET = docs/js/tetris.js

all: src/*.elm
	elm-make src/Tetris.elm --output=$(TARGET)
