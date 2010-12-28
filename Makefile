CONFIG_OPTS=--ghc-option=-Wall

warning:
	@echo "See README."

install-deps:
	cabal --version
	cabal install Vec ${OPTS}
	cabal install MaybeT ${OPTS}
	cabal install MonadRandom ${OPTS}
	cabal install data-inttrie ${OPTS}
	cabal install data-memocombinators ${OPTS}
	cabal install PSQueue ${OPTS}
	cabal install vector ${OPTS}
	cabal install statistics ${OPTS}
	cabal install priority-sync ${OPTS}
	cabal install Vec-OpenGLRaw ${OPTS}
	cabal install hslogger ${OPTS}

clean:
	(cd rsagl-math && cabal clean ${OPTS})
	(cd rsagl-frp && cabal clean ${OPTS})
	(cd rsagl && cabal clean ${OPTS})
	(cd rsagl-demos && cabal clean ${OPTS})
	(cd roguestar-engine && cabal clean ${OPTS})
	(cd roguestar-gl && cabal clean ${OPTS})
	rm -rf roguestar-sdist

install:
	(cd rsagl-math && cabal install --reinstall ${OPTS})
	(cd rsagl-frp && cabal install --reinstall ${OPTS})
	(cd rsagl && cabal install --reinstall ${OPTS})
	(cd rsagl-demos && cabal install --reinstall ${OPTS})
	(cd roguestar-engine && cabal install --reinstall ${OPTS})
	(cd roguestar-gl && cabal install --reinstall ${OPTS})

dev:
	${MAKE} install -e "OPTS=${CONFIG_OPTS}"

prof:
	${MAKE} install -e "OPTS=${CONFIG_OPTS} --enable-library-profiling --enable-executable-profiling"

sdist:
	(cd rsagl-math && cabal check && cabal sdist ${OPTS})
	(cd rsagl-frp && cabal check && cabal sdist ${OPTS})
	(cd rsagl && cabal check && cabal sdist ${OPTS})
	(cd rsagl-demos && cabal check && cabal sdist ${OPTS})
	(cd roguestar-engine && cabal check && cabal sdist ${OPTS})
	(cd roguestar-gl && cabal check && cabal sdist ${OPTS})
	mkdir -p ./roguestar-sdist
	cp rsagl-math/dist/*.tar.gz ./roguestar-sdist
	cp rsagl-frp/dist/*.tar.gz ./roguestar-sdist
	cp rsagl/dist/*.tar.gz ./roguestar-sdist
	cp rsagl-demos/dist/*.tar.gz ./roguestar-sdist
	cp roguestar-engine/dist/*.tar.gz ./roguestar-sdist
	cp roguestar-gl/dist/*.tar.gz ./roguestar-sdist
	(cd roguestar-sdist && tar xzf roguestar-engine-${VERSION}.tar.gz)
	(cd roguestar-sdist && tar xzf roguestar-gl-${VERSION}.tar.gz)
	(cd roguestar-sdist && tar xzf rsagl-math-${VERSION}.tar.gz)
	(cd roguestar-sdist && tar xzf rsagl-frp-${VERSION}.tar.gz)
	(cd roguestar-sdist && tar xzf rsagl-${VERSION}.tar.gz)
	(cd roguestar-sdist && tar xzf rsagl-demos-${VERSION}.tar.gz)
	(cd roguestar-sdist/roguestar-engine-${VERSION} && cabal configure && cabal install)
	(cd roguestar-sdist/rsagl-math-${VERSION} && cabal configure && cabal install)
	(cd roguestar-sdist/rsagl-frp-${VERSION} && cabal configure && cabal install)
	(cd roguestar-sdist/rsagl-${VERSION} && cabal configure && cabal install)
	(cd roguestar-sdist/rsagl-demos-${VERSION} && cabal configure && cabal install)
	(cd roguestar-sdist/roguestar-gl-${VERSION} && cabal configure && cabal install)
	ls roguestar-sdist

