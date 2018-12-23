TARGET = docs/js/tetris.js

all: src/*.elm
	elm make src/Main.elm --output=$(TARGET) --debug

opt: src/*.elm
	elm make src/Main.elm --output=$(TARGET) --optimize

clean:
	rm -f $(TARGET)

.PHONY: all opt clean