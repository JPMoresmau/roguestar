CONFIG_LIB_OPTS=--ghc-option=-Wall
CONFIG_BIN_OPTS=--prefix="${PWD}/roguestar-local" --ghc-option=-Wall

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
	-rm -rf ./roguestar-local
	-rm -rf ./roguestar-sdist
	${MAKE} clean-libs
	${MAKE} clean-bins

clean-libs:
	(cd rsagl-math && cabal clean ${OPTS})
	(cd rsagl-frp && cabal clean ${OPTS})
	(cd rsagl && cabal clean ${OPTS})

clean-bins:
	(cd rsagl-demos && cabal clean ${OPTS})
	(cd roguestar-engine && cabal clean ${OPTS})
	(cd roguestar-gl && cabal clean ${OPTS})

config-libs1:
	(cd rsagl-math && cabal configure --user ${CONFIG_LIB_OPTS} ${OPTS})

config-libs2:
	(cd rsagl-frp && cabal configure --user ${CONFIG_LIB_OPTS} ${OPTS})

config-libs3:
	(cd rsagl && cabal configure --user ${CONFIG_LIB_OPTS} ${OPTS})

config-bins:
	mkdir -p ./roguestar-local
	(cd rsagl-demos && cabal configure --user ${CONFIG_BIN_OPTS} ${OPTS})
	(cd roguestar-engine && cabal configure --user ${CONFIG_BIN_OPTS} ${OPTS})
	(cd roguestar-gl && cabal configure --user ${CONFIG_BIN_OPTS} ${OPTS})

build-libs1:
	(cd rsagl-math && cabal build ${OPTS})

build-libs2:
	(cd rsagl-frp && cabal build ${OPTS})

build-libs3:
	(cd rsagl && cabal build ${OPTS})

build-bins:
	(cd rsagl-demos && cabal build ${OPTS})
	(cd roguestar-engine && cabal build ${OPTS})
	(cd roguestar-gl && cabal build ${OPTS})

install-libs:
	(cd rsagl-math && cabal install --reinstall ${OPTS})
	(cd rsagl-frp && cabal install --reinstall ${OPTS})
	(cd rsagl && cabal install --reinstall ${OPTS})

install-bins:
	(cd rsagl-demos && cabal install --reinstall ${OPTS})
	(cd roguestar-engine && cabal install --reinstall ${OPTS})
	(cd roguestar-gl && cabal install --reinstall ${OPTS})

copy-libs1:
	(cd rsagl-math && cabal copy ${OPTS} && cabal register ${OPTS})

copy-libs2:
	(cd rsagl-frp && cabal copy ${OPTS} && cabal register ${OPTS})

copy-libs3:
	(cd rsagl && cabal copy ${OPTS} && cabal register ${OPTS})

copy-bins:
	(cd rsagl-demos && cabal copy ${OPTS})
	(cd roguestar-engine && cabal copy ${OPTS})
	(cd roguestar-gl && cabal copy ${OPTS})

install:
	${MAKE} install-libs
	${MAKE} install-bins

from-scratch:
	${MAKE} clean -e OPTS=""
	${MAKE} config-libs1 -e OPTS=""
	${MAKE} build-libs1 -e OPTS=""
	${MAKE} copy-libs1 -e OPTS=""
	${MAKE} config-libs2 -e OPTS=""
	${MAKE} build-libs2 -e OPTS=""
	${MAKE} copy-libs2 -e OPTS=""
	${MAKE} config-libs3 -e OPTS=""
	${MAKE} build-libs3 -e OPTS=""
	${MAKE} copy-libs3 -e OPTS=""
	${MAKE} config-bins -e OPTS=""
	${MAKE} build-bins -e OPTS=""
	${MAKE} copy-bins -e OPTS=""

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

