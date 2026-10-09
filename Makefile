.PHONY: all run app test clean
all:
	cabal build exe:hj
	mkdir -p build
	cp "$$(cabal list-bin exe:hj)" build/hj
run: all
	./build/hj
app: all
	sh scripts/macos-app.sh
test:
	cabal test --test-show-details=direct
clean:
	cabal clean
	rm -rf build
