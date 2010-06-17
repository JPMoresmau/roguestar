CONFIG_LIB_OPTS=--ghc-option=-Wall
CONFIG_BIN_OPTS=--prefix=./roguestar-local --ghc-option=-Wall

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

clean:
	-rm -rf ./roguestar-local
	-rm -rf ./roguestar-sdist
	${MAKE} clean-libs
	${MAKE} clean-bins

clean-libs:
	(cd rsagl && cabal clean ${OPTS})

clean-bins:
	(cd rsagl-demos && cabal clean ${OPTS})
	(cd roguestar-engine && cabal clean ${OPTS})
	(cd roguestar-gl && cabal clean ${OPTS})

config-libs:
	(cd rsagl && cabal configure --user ${CONFIG_LIB_OPTS} ${OPTS})

config-bins:
	mkdir -p ./roguestar-local
	(cd rsagl-demos && cabal configure --user ${CONFIG_BIN_OPTS} ${OPTS})
	(cd roguestar-engine && cabal configure --user ${CONFIG_BIN_OPTS} ${OPTS})
	(cd roguestar-gl && cabal configure --user ${CONFIG_BIN_OPTS} ${OPTS})

build-libs:
	(cd rsagl && cabal build ${OPTS})

build-bins:
	(cd rsagl-demos && cabal build ${OPTS})
	(cd roguestar-engine && cabal build ${OPTS})
	(cd roguestar-gl && cabal build ${OPTS})

copy-libs:
	(cd rsagl && cabal copy ${OPTS})

copy-bins:
	(cd rsagl-demos && cabal copy ${OPTS})
	(cd roguestar-engine && cabal copy ${OPTS})
	(cd roguestar-gl && cabal copy ${OPTS})

install-libs:
	(cd rsagl && cabal install --reinstall ${OPTS})

install-bins:
	(cd rsagl-demos && cabal install --reinstall ${OPTS})
	(cd roguestar-engine && cabal install --reinstall ${OPTS})
	(cd roguestar-gl && cabal install --reinstall ${OPTS})

install:
	${MAKE} install-libs
	${MAKE} install-bins

from-scratch:
	${MAKE} clean -e OPTS=""
	${MAKE} config-libs -e OPTS=""
	${MAKE} build-libs -e OPTS=""
	${MAKE} copy-libs -e OPTS=""
	${MAKE} config-bins -e OPTS=""
	${MAKE} build-bins -e OPTS=""
	${MAKE} copy-bins -e OPTS=""

from-libs:
	${MAKE} build-libs -e OPTS=""
	${MAKE} copy-libs -e OPTS=""
	${MAKE} clean-bins -e OPTS=""
	${MAKE} config-bins -e OPTS=""
	${MAKE} build-bins -e OPTS=""
	${MAKE} copy-bins -e OPTS=""

sdist:
	(cd rsagl && cabal check && cabal sdist ${OPTS})
	(cd rsagl-demos && cabal check && cabal sdist ${OPTS})
	(cd roguestar-engine && cabal check && cabal sdist ${OPTS})
	(cd roguestar-gl && cabal check && cabal sdist ${OPTS})
	mkdir -p ./roguestar-sdist
	cp rsagl/dist/*.tar.gz ./roguestar-sdist
	cp rsagl-demos/dist/*.tar.gz ./roguestar-sdist
	cp roguestar-engine/dist/*.tar.gz ./roguestar-sdist
	cp roguestar-gl/dist/*.tar.gz ./roguestar-sdist
	ls roguestar-sdist

