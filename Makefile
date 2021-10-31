BUILD_DIR=_out
STATIC_DIR=static
SRC_DIR=src
SASS_DIR=sass
SASS_LIB_DIR=${SASS_DIR}/lib
SASS_VENDOR_DIR=${SASS_DIR}/vendor/
SASS_CMD = /usr/bin/sass

DUNE_PREFIX=_build/default
JS_TARGET=${SRC_DIR}/tiny_cc.bc.js
JS_FILE=${DUNE_PREFIX}/${JS_TARGET}
SASS_FILE=${SASS_DIR}/tiny.scss

JS_BUILD=${BUILD_DIR}/tiny_cc.js
CSS_BUILD=${BUILD_DIR}/tiny.css

OUT_FILE=docs/index.html

-include local.Makefile

.PHONY: all static js html gen outdir

all: html

css: ${BUILD_DIR} ${SASS_DIR}/*
	${SASS_CMD} -I ${SASS_VENDOR_DIR} -I ${SASS_LIB_DIR} --style=compressed ${SASS_FILE} ${BUILD_DIR}/tiny.css

js: ${BUILD_DIR} ${JS_TARGET}
	rm -f ${BUILD_DIR}/tiny_cc.js
	cp ${JS_FILE} ${BUILD_DIR}/tiny_cc.js

html: js css gen outdir
	dune exe gen/gen.exe -- --script=${JS_BUILD} --style=${CSS_BUILD} -o ${OUT_FILE}

gen:
	dune build gen/gen.exe

${JS_TARGET}:
	dune build $@

${BUILD_DIR}:
	mkdir -p ${BUILD_DIR}

outdir:
	mkdir -p docs

clean:
	dune clean
	rm -rf ${BUILD_DIR}/*
