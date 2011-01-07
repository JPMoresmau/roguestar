CONFIG_OPTS=--ghc-option=-Wall

warning:
	@echo "See README."

install-deps:
	cabal --version
	cabal install --reinstall Vec ${OPTS}
	cabal install --reinstall MaybeT ${OPTS}
	cabal install --reinstall MonadRandom ${OPTS}
	cabal install --reinstall data-inttrie ${OPTS}
	cabal install --reinstall data-memocombinators ${OPTS}
	cabal install --reinstall PSQueue ${OPTS}
	cabal install --reinstall vector ${OPTS}
	cabal install --reinstall statistics ${OPTS}
	cabal install --reinstall priority-sync ${OPTS}
	cabal install --reinstall Vec-OpenGLRaw ${OPTS}
	cabal install --reinstall hslogger ${OPTS}

clean:
	(cd rsagl-math && cabal clean ${OPTS})
	(cd rsagl-frp && cabal clean ${OPTS})
	(cd rsagl && cabal clean ${OPTS})
	(cd rsagl-demos && cabal clean ${OPTS})
	(cd roguestar-engine && cabal clean ${OPTS})
	(cd roguestar-gl && cabal clean ${OPTS})
	(cd roguestar-glut && cabal clean ${OPTS})
	(cd roguestar-gtk && cabal clean ${OPTS})
	(cd roguestar && cabal clean ${OPTS})
	rm -rf roguestar-sdist

install: roguestar roguestar-gtk roguestar-glut roguestar-engine rsagl-demos

rsagl-math:
	(cd rsagl-math && cabal install --reinstall ${OPTS})

rsagl-frp: rsagl-math
	(cd rsagl-frp && cabal install --reinstall ${OPTS})

rsagl: rsagl-frp rsagl-math
	(cd rsagl && cabal install --reinstall ${OPTS})

rsagl-demos: rsagl
	(cd rsagl-demos && cabal install --reinstall ${OPTS})

roguestar-engine:
	(cd roguestar-engine && cabal install --reinstall ${OPTS})

roguestar-gl: rsagl
	(cd roguestar-gl && cabal install --reinstall ${OPTS})

roguestar-glut: roguestar-gl
	(cd roguestar-glut && cabal install --reinstall ${OPTS})

roguestar-gtk: roguestar-gl
	(cd roguestar-gtk && cabal install --reinstall ${OPTS})

roguestar:
	(cd roguestar && cabal install --reinstall ${OPTS})

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
	(cd roguestar-glut && cabal check && cabal sdist ${OPTS})
	(cd roguestar-gtk && cabal check && cabal sdist ${OPTS})
	(cd roguestar && cabal check && cabal sdist ${OPTS})
	mkdir -p ./roguestar-sdist
	cp rsagl-math/dist/*.tar.gz ./roguestar-sdist
	cp rsagl-frp/dist/*.tar.gz ./roguestar-sdist
	cp rsagl/dist/*.tar.gz ./roguestar-sdist
	cp rsagl-demos/dist/*.tar.gz ./roguestar-sdist
	cp roguestar-engine/dist/*.tar.gz ./roguestar-sdist
	cp roguestar-gl/dist/*.tar.gz ./roguestar-sdist
	cp roguestar-glut/dist/*.tar.gz ./roguestar-sdist
	cp roguestar-gtk/dist/*.tar.gz ./roguestar-sdist
	cp roguestar/dist/*.tar.gz ./roguestar-sdist
	(cd roguestar-sdist && tar xzf roguestar-engine-${VERSION}.tar.gz)
	(cd roguestar-sdist && tar xzf roguestar-gl-${VERSION}.tar.gz)
	(cd roguestar-sdist && tar xzf roguestar-glut-${VERSION}.tar.gz)
	(cd roguestar-sdist && tar xzf roguestar-gtk-${VERSION}.tar.gz)
	(cd roguestar-sdist && tar xzf roguestar-${VERSION}.tar.gz)
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
	(cd roguestar-sdist/roguestar-glut-${VERSION} && cabal configure && cabal install)
	(cd roguestar-sdist/roguestar-gtk-${VERSION} && cabal configure && cabal install)
	(cd roguestar-sdist/roguestar-${VERSION} && cabal configure && cabal install)
	ls roguestar-sdist

.PHONY: rsagl-math rsagl-frp rsagl rsagl-demos roguestar-engine roguestar-gl roguestar-glut roguestar-gtk roguestar
