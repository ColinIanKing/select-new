SRC=tools.ml commits.ml report.ml binding.ml filters.ml select_main.ml
NATIVE_OBJS=$(SRC:.ml=.cmx)
NATIVE_OBJS_COV=$(SRC:%.ml=lib/coverage/%.cmx)

PARMAP_BUILD_DIR=parmap/_build
PARMAP_NATIVE_LIB=parmap.cmxa

GCC_REDUCE_BUILD_DIR=gcc-reduce
GCC_REDUCE_NATIVE_OBJS=common.cmx options.cmx lines.cmx \
	types.cmx generate.cmx rules2.cmx read.cmx

COVERAGE_NATIVE_DIR=lib/coverage


EXEC=select_drivers

all: $(EXEC)

select_drivers: $(SRC) parmap gcc-reduce
	ocamlopt -g -o $(EXEC) \
	-I $(PARMAP_BUILD_DIR) -I $(GCC_REDUCE_BUILD_DIR) \
	str.cmxa nums.cmxa unix.cmxa bigarray.cmxa \
	$(PARMAP_NATIVE_LIB) \
	$(GCC_REDUCE_NATIVE_OBJS) \
	$(SRC)

gcc-reduce: gcc-reduce/gcc-reduce.opt

gcc-reduce/gcc-reduce.opt:
	cd gcc-reduce && $(MAKE) opt

parmap: parmap/_build/parmap.cmxa

parmap/_build/parmap.cmxa: parmap/Makefile
	cd parmap && $(MAKE)

parmap/Makefile:
	cd parmap && ./configure

process: processErrors.ml
	ocamlopt  -o process unix.cmxa str.cmxa processErrors.ml

report: report.ml report_main.ml
	ocamlopt -o report str.cmxa report.ml report_main.ml

select_sp: select_sp.ml
	ocamlopt -o select_sp str.cmxa unix.cmxa select_sp.ml

lib/coverage/%.cmx: %.ml
	ocamlfind opt -package bisect_ppx -c $< -o $@ \
	-I $(PARMAP_BUILD_DIR) -I $(GCC_REDUCE_BUILD_DIR) \
	-I $(COVERAGE_NATIVE_DIR)

coverage: $(NATIVE_OBJS_COV)
	ocamlfind opt -linkpkg -package bisect_ppx \
	-I $(PARMAP_BUILD_DIR) -I $(GCC_REDUCE_BUILD_DIR) \
	str.cmxa nums.cmxa unix.cmxa bigarray.cmxa \
	$(PARMAP_NATIVE_LIB) \
	$(GCC_REDUCE_NATIVE_OBJS) \
	$(NATIVE_OBJS_COV) -o $(EXEC)_coverage
