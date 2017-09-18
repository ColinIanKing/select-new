all: select_drivers

select_drivers: select_main.ml tools.ml commits.ml binding.ml report process gcc-reduce parmap
	ocamlopt -g -o select_drivers \
	-I parmap/_build -I gcc-reduce \
	str.cmxa nums.cmxa unix.cmxa bigarray.cmxa \
	parmap.cmxa \
	common.cmx options.cmx lines.cmx types.cmx generate.cmx rules2.cmx read.cmx \
	tools.ml commits.ml report.ml binding.ml filters.ml select_main.ml

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
