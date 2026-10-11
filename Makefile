.PHONY: all run app test render-test clean
all:
	cabal build exe:unholy
	mkdir -p build
	cp "$$(cabal list-bin exe:unholy)" build/unholy
run: all
	./build/unholy
app: all
	sh scripts/macos-app.sh
test:
	cabal test unholy-tests pulse-courier-tests circuit-breaker-tests threshold-tests --test-show-details=direct
render-test:
	cabal test unholy-renderer-tests --test-show-details=direct
clean:
	cabal clean
	rm -rf build
