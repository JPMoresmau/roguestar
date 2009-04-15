GIT_CHECKOUT=master
VERSION=${GIT_CHECKOUT}
VERSION_SUFFIX=-${VERSION}
PACKAGE_DB=--package-db="${PWD}/roguestar-local/cabal-package-db" 
CONFIGURE_EXTRA_OPTS=--ghc-option=-Werror
CONFIGURE_OPTS=${PACKAGE_DB} --prefix="${PWD}/roguestar-local"

# Change these to make dist from somewhere other than the downstairspeople.org repo.
GIT_ORIGIN_PATH=http://www.downstairspeople.org/git
GIT_ORIGIN_SUFFIX=.git

make : roguestar-gl roguestar-engine rsagl-doc

setup :
	-rm -rf roguestar-local
	mkdir roguestar-local
	echo "[]" > roguestar-local/cabal-package-db
	cabal install MaybeT ${CONFIGURE_OPTS}
	cabal install MonadRandom ${CONFIGURE_OPTS}
	cabal install parsec ${CONFIGURE_OPTS} --preference='parsec >= 3'
	cabal install QuickCheck ${CONFIGURE_OPTS} --preference='QuickCheck < 2'
	(cd roguestar-engine && runghc Setup.hs clean && runghc Setup.hs configure ${CONFIGURE_OPTS} ${CONFIGURE_EXTRA_OPTS})
	(cd rsagl && runghc Setup.hs clean && runghc Setup.hs configure ${CONFIGURE_OPTS} ${CONFIGURE_EXTRA_OPTS})
	(cd rsagl && runghc Setup.hs build && runghc Setup.hs install)
	(cd roguestar-gl && runghc Setup.hs clean && runghc Setup.hs configure ${CONFIGURE_OPTS} ${CONFIGURE_EXTRA_OPTS})

rsagl :
	(cd rsagl && runghc Setup.hs build && runghc Setup.hs install)

roguestar-gl : rsagl
	(cd roguestar-gl && runghc Setup.hs build && runghc Setup.hs install)

roguestar-engine :
	(cd roguestar-engine && runghc Setup.hs build && runghc Setup.hs install)

rsagl-doc :
	(cd rsagl && runghc Setup.hs haddock --internal)

from-scratch :
	${MAKE} setup
	${MAKE}

clean :
	-rm -rf roguestar-local
	-rm -rf dist
	-rm -rf dist-test
	-(cd roguestar-engine && runghc Setup.hs clean)
	-(cd roguestar-gl && runghc Setup.hs clean)
	-(cd rsagl && runghc Setup.hs clean)

dist :
	rm -rf dist
	rm -rf dist-test
	mkdir -p dist/roguestar${VERSION_SUFFIX}
	git clone -q ${GIT_ORIGIN_PATH}/roguestar-engine${GIT_ORIGIN_SUFFIX} dist/roguestar${VERSION_SUFFIX}/roguestar-engine
	git clone -q ${GIT_ORIGIN_PATH}/roguestar-gl${GIT_ORIGIN_SUFFIX} dist/roguestar${VERSION_SUFFIX}/roguestar-gl
	git clone -q ${GIT_ORIGIN_PATH}/rsagl${GIT_ORIGIN_SUFFIX} dist/roguestar${VERSION_SUFFIX}/rsagl
	(cd dist/roguestar${VERSION_SUFFIX}/rsagl && git checkout ${GIT_CHECKOUT} && rm -rf .git)
	(cd dist/roguestar${VERSION_SUFFIX}/roguestar-gl && git checkout ${GIT_CHECKOUT} && rm -rf .git)
	(cd dist/roguestar${VERSION_SUFFIX}/roguestar-engine && git checkout ${GIT_CHECKOUT} && rm -rf .git)
	(cd dist; tar c roguestar${VERSION_SUFFIX}/ | gzip -9 > roguestar${VERSION_SUFFIX}.tar.gz )
	(cd dist/roguestar${VERSION_SUFFIX}/roguestar-engine && runghc Setup.hs sdist && cp dist/roguestar-engine-${VERSION}.tar.gz ../..)
	(cd dist/roguestar${VERSION_SUFFIX}/roguestar-gl && runghc Setup.hs sdist && cp dist/roguestar-gl-${VERSION}.tar.gz ../..)
	(cd dist/roguestar${VERSION_SUFFIX}/rsagl && runghc Setup.hs sdist && cp dist/rsagl-${VERSION}.tar.gz ../..)
	mkdir -p dist-test/src
	echo "[]" > "${PWD}/dist-test/cabal-package-db"
	cp dist/roguestar-engine-${VERSION}.tar.gz dist-test/src/
	cp dist/roguestar-gl-${VERSION}.tar.gz dist-test/src/
	cp dist/rsagl-${VERSION}.tar.gz dist-test/src/
	(cd dist-test/src && tar xzf roguestar-engine-${VERSION}.tar.gz && cd roguestar-engine-${VERSION} && \
            runghc Setup.hs configure --disable-optimization --package-db="${PWD}/dist-test/cabal-package-db" --prefix="${PWD}/dist-test" \
	    && runghc Setup.hs build && runghc Setup.hs install)
	(cd dist-test/src && tar xzf rsagl-${VERSION}.tar.gz && cd rsagl-${VERSION} &&                       \
            runghc Setup.hs configure --disable-optimization --package-db="${PWD}/dist-test/cabal-package-db" --prefix="${PWD}/dist-test" && \
	    runghc Setup.hs build && runghc Setup.hs install)
	(cd dist-test/src && tar xzf roguestar-gl-${VERSION}.tar.gz && cd roguestar-gl-${VERSION} &&                \
            runghc Setup.hs configure --disable-optimization --package-db="${PWD}/dist-test/cabal-package-db" --prefix="${PWD}/dist-test" && \
	    runghc Setup.hs build && runghc Setup.hs install)
	(dist-test/bin/roguestar-engine version | grep ${VERSION})
	(cd dist/roguestar${VERSION_SUFFIX}/roguestar-engine && cabal check)
	(cd dist/roguestar${VERSION_SUFFIX}/roguestar-gl && cabal check)
	(cd dist/roguestar${VERSION_SUFFIX}/rsagl && cabal check)
	@echo "This distribution looks good.  Try dist-test/bin/roguestar."

dist-snapshot :
	${MAKE} dist -e VERSION_SUFFIX="-${GIT_CHECKOUT}-`date --utc +%Y%m%d%H%M`"

dist-local :
	${MAKE} dist-snapshot -e GIT_ORIGIN_PATH=. -e GIT_ORIGIN_SUFFIX=/.git

.PHONY: default download make setup-clean dist dist-snapshot roguestar-engine roguestar-gl rsagl from-scratch rsagl-doc
