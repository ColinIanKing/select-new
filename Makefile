select_drivers: select_main.ml tools.ml commits.ml report.ml binding.ml process gcc-reduce
	ocamlopt -g -o select_drivers \
	str.cmxa nums.cmxa unix.cmxa bigarray.cmxa \
	parmap/_build/parmap.cmxa \
	-I parmap/_build \
	tools.ml commits.ml report.ml \
	-I gcc-reduce/ gcc-reduce/common.cmx gcc-reduce/options.cmx \
	gcc-reduce/lines.cmx gcc-reduce/types.cmx gcc-reduce/generate.cmx \
	gcc-reduce/rules2.cmx gcc-reduce/read.cmx \
	binding.ml select_main.ml

process: processErrors.ml
	ocamlopt  -o process unix.cmxa str.cmxa processErrors.ml

report: report.ml report_main.ml
	ocamlopt -o report str.cmxa report.ml report_main.ml

select_sp: select_sp.ml
	ocamlopt -o select_sp str.cmxa unix.cmxa select_sp.ml

runall: runall.ml
	ocamlc -o runall \
	str.cma nums.cma unix.cma bigarray.cma \
	`ocamlfind query parmap`/parmap.cma \
	-I `ocamlfind query parmap` runall.ml

analyze_git: analyze_git.ml
	ocamlc -g -o analyze_git \
	str.cma nums.cma unix.cma analyze_git.ml
	cd 2013_results ; ../analyze_git ; \
	cd ../2015_results ; ../analyze_git

commits_and_times: commits_and_times.ml
	ocamlc -o commits_and_times \
	str.cma nums.cma unix.cma commits_and_times.ml
	cd 2013_results ; ../commits_and_times > commits_and_times.tex ; \
	cd ../2015_results ; ../commits_and_times > commits_and_times.tex

reduction_to_500: reduction_to_500.ml
	ocamlc -o reduction_to_500 \
	str.cma nums.cma unix.cma reduction_to_500.ml
	./reduction_to_500 > reduction_to_500_res

services_vs_error_types: services_vs_error_types.ml
	ocamlc -g -o services_vs_error_types \
	str.cma nums.cma unix.cma services_vs_error_types.ml
	./services_vs_error_types > services_vs_error_types.tex

error_types_vs_years: error_types_vs_years.ml
	ocamlc -g -o error_types_vs_years \
	str.cma nums.cma unix.cma error_types_vs_years.ml
	./error_types_vs_years > error_types_vs_years.tex

error_types_vs_runtime: error_types_vs_runtime.ml
	ocamlc -g -o error_types_vs_runtime \
	str.cma nums.cma unix.cma error_types_vs_runtime.ml
	./error_types_vs_runtime > error_types_vs_runtime.tex

one: select_drivers
	./select_drivers --start "Jan 1, 2015" --end "Dec 31, 2015" > res2015

two: select_drivers
	./select_drivers --start "Jan 1, 2014" --end "Dec 31, 2014" > res2014

three: select_drivers
	./select_drivers --start "Jan 1, 2013" --end "Dec 31, 2013" > res2013

all: one two three

onea: select_drivers
	./select_drivers --avg --start "Jan 1, 2015" --end "Dec 31, 2015" > \
	resa2015

twoa: select_drivers
	./select_drivers --avg --start "Jan 1, 2014" --end "Dec 31, 2014" > \
	resa2014

threea: select_drivers
	./select_drivers --avg --start "Jan 1, 2013" --end "Dec 31, 2013" > \
	resa2013

alla: onea twoa threea

onecc: select_drivers report
	`pwd`/select_drivers --start "Jan 1, 2015" \
	--end "Dec 31, 2015" --key 2015c --cores 18 \
	--git /mnt/ramdisk/linux > rescc2015c

onecc1: select_drivers report
	`pwd`/select_drivers --start "Jan 1, 2015" \
	--end "Dec 31, 2015" --key 2015c --cores 1 \
	--git /mnt/ramdisk/linux > rescc2015c

twocc: select_drivers report
	`pwd`/select_drivers --start "Jan 1, 2014" \
	--end "Dec 31, 2014" --key 2014c --cores 18 \
	--git /mnt/ramdisk/linux > rescc2014c

threecc: select_drivers report
	`pwd`/select_drivers --start "Jan 1, 2013" \
	--end "Dec 31, 2013" --key 2013c --cores 1 \
	--git /tmp/ram/linux-git > rescc2013c

threecc_redo: select_drivers report
	`pwd`/select_drivers --start "Jan 1, 2013" \
	--end "Dec 31, 2013" --key 2013c_redo --cores 18 \
	--git /mnt/ramdisk/linux > rescc2013c_redo

onebc: select_drivers report
	`pwd`/select_drivers --start "Jan 1, 2015" \
	--end "Dec 31, 2015" --key 2015bc --cores 18 --backport \
	--git /mnt/ramdisk/linux > rescc2015bc

twobc: select_drivers report
	`pwd`/select_drivers --start "Jan 1, 2014" \
	--end "Dec 31, 2014" --key 2014bc --cores 18 --backport \
	--git /mnt/ramdisk/linux > rescc2014bc

threebc: select_drivers report
	`pwd`/select_drivers --start "Jan 1, 2013" \
	--end "Dec 31, 2013" --key 2013bc --cores 18 --backport \
	--git /mnt/ramdisk/linux > rescc2013bc

threecc1: select_drivers
	`pwd`/select_drivers --start "Jan 1, 2013" \
	--end "Dec 31, 2013" --key 2013c --cores 1 --llvm \
	--git /mnt/ramdisk/linux > rescc2013c

janfeb: select_drivers
	`pwd`/select_drivers  --start "Jan 1, 2013" \
	--end "Feb 28, 2013" \
	--key janfeb --cores 18 --git /mnt/ramdisk/linux > janfebres

tiny: select_drivers
	`pwd`/select_drivers --range a2871c6^..a2871c6 \
	--key mytinyc --cores 18 --git /mnt/ramdisk/linux > tinyresc

tinyb: select_drivers
	`pwd`/select_drivers --range 456930d^..456930d --backport \
	--key mytinybc --cores 18 --git /mnt/ramdisk/linux > tinyresbc



usenix13f: select_drivers
	`pwd`/select_drivers --list e268395,e809c22,217494e,7be56a8,ce77399,09a642b,26780d9,e4fc408,7cbe0ff,62e00cb \
	--key usenix13f --cores 18 --git /mnt/ramdisk/linux > usenix13fres

usenix13b: select_drivers
	`pwd`/select_drivers --list e268395,e809c22,217494e,7be56a8,ce77399,09a642b,26780d9,e4fc408,7cbe0ff,62e00cb --backport \
	--key usenix13b --cores 18 --git /mnt/ramdisk/linux > usenix13bres

usenix13: usenix13f usenix13b 

usenix15f: select_drivers
	`pwd`/select_drivers \
	--list 1c14905,744543c,b7da8c5,f031237,ea98b29 \
	--key usenix15f --cores 1 --git /tmp/ram/linux-git > usenix15fres



usenix13f_more_for_final: select_drivers
	`pwd`/select_drivers --list f1a18a1,41c47d8,8735a81,bf29fbe,8e84c25,1d09aaa,872e79a,da0a00e,6f8da5d,ca3de46 \
	--key usenix13f_more_for_final --cores 18 --git /mnt/ramdisk/linux \
	> usenix13f_more_for_finalres

usenix13b_more_for_final: select_drivers
	`pwd`/select_drivers --list f1a18a1,41c47d8,8735a81,bf29fbe,8e84c25,1d09aaa,872e79a,da0a00e,6f8da5d,ca3de46 --backport \
	--key usenix13b_more_for_final --cores 18 --git /mnt/ramdisk/linux \
	> usenix13b_more_for_finalres


usenix15f_more_for_final: select_drivers
	`pwd`/select_drivers --list 1ceacea,c0c050c,44e259a,6234f38,c610afa \
	--key usenix15f_more_for_final --cores 18 --git /mnt/ramdisk/linux > usenix15f_more_for_finalres



allcc: onecc twocc threecc

clean:
	/bin/rm -rf 2013 2014 2015 rescc2013 rescc2014 rescc2015

clean_all:
	/bin/rm -rf 2013 2014 2015 \
	2013_files 2014_files 2015_files rescc2013 rescc2014 rescc2015
