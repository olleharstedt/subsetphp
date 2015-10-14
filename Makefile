all: clear subsetphp test llvm_test

clear:
	clear

subsetphp: realpath.o hh_shared.o parser_hack.cmx infer.cmx main.ml
	ocamlfind ocamlopt -g -w @5 -package ppx_deriving.show -linkall ident.cmx utils.cmx unix.cmxa str.cmxa sys_utils.cmx path.cmx relative_path.cmx pos.cmx errors.cmx lexer_hack.cmx namespace_env.cmx lint.cmx prefix.cmx eventLogger.cmx realpath.o hh_shared.o sharedMem.cmx parser_heap.cmx namespaces.cmx parser_hack.cmx fileInfo.cmx ast.cmx typedast.cmx infer.cmx main.ml -o subsetphp

parser_hack.cmx: lint.cmx lexer_hack.cmx parser_heap.cmx typedast.cmx ast.cmx namespaces.cmx parser_hack.cmi parser_hack.ml
	ocamlopt -c ast.cmx namespaces.cmx ide.cmx parser_hack.ml

parser_hack.cmi: parser_hack.mli
	ocamlopt -c ast.cmx parser_hack.mli

namespaces.cmi: namespaces.mli
	ocamlopt -c ast.cmx namespaces.mli

namespaces.cmx: utils.cmx namespaces.cmi namespaces.ml
	ocamlopt -c ast.cmx namespaces.ml

parser_heap.cmx: ast.cmx errors.cmx pos.cmx sharedMem.cmx parser_heap.ml
	ocamlopt -c prefix.cmx pos.cmx sharedMem.cmx parser_heap.ml

sharedMem.cmi: utils.cmx sharedMem.mli
	ocamlopt -c utils.cmx sharedMem.mli

sharedMem.cmx: value.cmx eventLogger.cmx sharedMem.cmi sharedMem.ml
	ocamlopt -c value.cmx eventLogger.cmx sharedMem.ml

value.cmi: prefix.cmx value.mli
	ocamlopt -c prefix.cmx value.mli

value.cmx: prefix.cmx value.cmi value.ml
	ocamlopt -c prefix.cmx value.ml

prefix.cmi: prefix.mli
	ocamlopt -c prefix.mli

prefix.cmx: prefix.cmi prefix.ml
	ocamlopt -c prefix.ml

ast.cmx: fileInfo.cmx pos.cmx namespace_env.cmx ast.ml
	ocamlfind ocamlopt -package ppx_deriving.show -c fileInfo.cmx namespace_env.cmx ast.ml

typedast.cmx: pos.cmx ast.cmx typedast.ml
	ocamlfind ocamlopt -package ppx_deriving.show -c fileInfo.cmx namespace_env.cmx pos.cmx ast.cmx typedast.ml

namespace_env.cmx: utils.cmx namespace_env.ml
	ocamlfind ocamlopt -package ppx_deriving.show -c namespace_env.ml

lexer_hack.cmx: lexer_hack.ml
	ocamlfind ocamlopt -c lexer_hack.ml

lexer_hack.ml: utils.cmx pos.cmx errors.cmx lexer_hack.mll
	ocamllex lexer_hack.mll

utils.cmx: ident.cmx utils.ml
	ocamlopt -c ident.cmx utils.ml

ident.cmx: ident.ml
	ocamlopt -c ident.ml

eventLogger.cmx: eventLogger.ml
	ocamlopt -c eventLogger.ml

pos.cmx: utils.cmx relative_path.cmx hh_json.cmx pos.ml
	ocamlfind ocamlopt -package ppx_deriving.show -c pos.ml

relative_path.cmx: path.cmx relative_path.ml
	ocamlopt -c relative_path.ml

path.cmx: path.cmi sys_utils.cmx path.ml
	ocamlopt -c path.ml

path.cmi: path.mli
	ocamlopt -c path.mli

sys_utils.cmx: sys_utils.ml
	ocamlopt -c sys_utils.ml

hh_json.cmx: core.cmx hh_json.ml
	ocamlopt -c hh_json.ml

core.cmx: core_list.cmx core.ml
	ocamlopt -c core.ml

core_list.cmx: poly.cmx container.cmx monad.cmx caml.cmx core_printf.cmx core_list.ml
	ocamlopt -c core_list.ml

core_printf.cmx: core_printf.ml
	ocamlopt -c core_printf.ml

caml.cmx: caml.ml
	ocamlopt -c caml.ml

monad.cmx: monad.ml
	ocamlopt -c monad.ml

container.cmx: commutative_group.cmx container.ml
	ocamlopt -c container.ml

commutative_group.cmx: commutative_group.ml
	ocamlopt -c commutative_group.ml

poly.cmx: polymorphic_compare.cmx poly.ml
	ocamlopt -c poly.ml

polymorphic_compare.cmi: polymorphic_compare.mli
	ocamlopt -c polymorphic_compare.mli

polymorphic_compare.cmx: polymorphic_compare.cmi polymorphic_compare.ml
	ocamlopt -c polymorphic_compare.ml

errors.cmi: errors.mli
	ocamlopt -c errors.mli

errors.cmx: pos.cmx errors.cmi errors.ml
	ocamlopt -c errors.ml

fileInfo.cmi: pos.cmx utils.cmx fileInfo.mli
	ocamlfind ocamlopt -package ppx_deriving.show -c fileInfo.mli

fileInfo.cmx: fileInfo.cmi fileInfo.ml
	ocamlfind ocamlopt -package ppx_deriving.show -c fileInfo.ml

lint.cmi: pos.cmx lint.mli
	ocamlopt -c lint.mli

lint.cmx: lint.cmi errors.cmx lint.ml
	ocamlopt -c lint.ml

hh_shared.o: hh_shared.c
	gcc -c hh_shared.c

realpath.o: realpath.c
	gcc -c realpath.c

type_test: utils.cmx path.cmx relative_path.cmx pos.cmx namespace_env.cmx fileInfo.cmx ast.cmx type.ml
	ocamlfind ocamlopt -package ppx_deriving.show ident.cmx utils.cmx unix.cmxa str.cmxa sys_utils.cmx path.cmx relative_path.cmx pos.cmx errors.cmx lexer_hack.cmx namespace_env.cmx lint.cmx prefix.cmx eventLogger.cmx realpath.o hh_shared.o sharedMem.cmx parser_heap.cmx namespaces.cmx parser_hack.cmx fileInfo.cmx ast.cmx type.ml -o type_test

infer.cmx: typedast.cmx infer.ml
	ocamlfind ocamlopt -g -w @5 -package ppx_deriving.show ident.cmx utils.cmx unix.cmxa str.cmxa sys_utils.cmx path.cmx relative_path.cmx pos.cmx errors.cmx lexer_hack.cmx namespace_env.cmx lint.cmx prefix.cmx eventLogger.cmx realpath.o hh_shared.o sharedMem.cmx parser_heap.cmx namespaces.cmx parser_hack.cmx fileInfo.cmx ast.cmx typedast.cmx infer.ml -o type_test2

test: typedast.cmx subsetphp test.ml
	ocamlfind ocamlopt -g -w @5 -linkpkg -package ppx_deriving.show,oUnit ident.cmx utils.cmx str.cmxa sys_utils.cmx path.cmx relative_path.cmx pos.cmx errors.cmx lexer_hack.cmx namespace_env.cmx lint.cmx prefix.cmx eventLogger.cmx realpath.o hh_shared.o sharedMem.cmx parser_heap.cmx namespaces.cmx parser_hack.cmx fileInfo.cmx ast.cmx typedast.cmx infer.cmx test.ml -o test

# First runtime test with OCaml GC, much harder than first though
# Not possible to make platform independent either
runtime.o: bindings.c
	#clang-3.6 -g -I php-src/Zend -I php-src -I php-src/TSRM -I php-src/main -I ocaml -I ocaml/byterun -I ocaml/asmrun php-src/Zend/*.o ocaml/byterun/*.o -o runtime.o bindings.c -lm -ldl -lncurses
	clang-3.6 -c -g -I php-src/Zend -I php-src -I php-src/TSRM -I php-src/main -I ocaml -I ocaml/byterun -I ocaml/asmrun -o runtime.o bindings.c

# Second GC try with semi-space copying GC and explicit shadow-stack
# GC code from semigc
runtime2.o: bindings2.c semigc
	clang-3.6 -c -g -I php-src/Zend -I php-src -I php-src/TSRM -I php-src/main -I ocaml -I ocaml/byterun -I ocaml/asmrun -o runtime2.o bindings2.c

llvm_test: runtime.o subsetphp typedast.cmx llvm_test.ml
	ocamlfind ocamlopt -g -w @5 -cc g++ -cc -lncurses -cclib -lffi -I /home/olle/.opam/4.02.1/llvm/ -I ocaml/asmrun -cc g++ -package llvm,llvm.bitreader,llvm.bitwriter,llvm.target,llvm.analysis,llvm.scalar_opts -linkpkg ident.cmx utils.cmx str.cmxa sys_utils.cmx path.cmx relative_path.cmx pos.cmx errors.cmx lexer_hack.cmx namespace_env.cmx lint.cmx prefix.cmx eventLogger.cmx realpath.o hh_shared.o sharedMem.cmx parser_heap.cmx namespaces.cmx parser_hack.cmx fileInfo.cmx ast.cmx typedast.cmx infer.cmx php-src/Zend/*.o ocaml/byterun/startup_aux.o ocaml/byterun/misc.o runtime.o llvm_test.ml -o llvm_test

llvm_test_compile: llvm_test
	./llvm_test
	llvm-dis-3.6 llvm_test.bc
	llc-3.6 llvm_test.bc
	clang-3.6 -g -c llvm_test.s
	clang-3.6 -g -I php-src/Zend -o test php-src/Zend/*.o ocaml/byterun/*.o llvm_test.o semigc/alloc.o -O3 -lm -ldl -lncurses

ll2: runtime2.o semigc
	llc-3.6 llvm_test.ll
	clang-3.6 -g -c llvm_test.s
	clang-3.6 -g -I php-src/Zend -o test php-src/Zend/*.o ocaml/byterun/*.o llvm_test.o semigc/alloc.o runtime2.o -O3 -lm -ldl -lncurses

ll: llvm_test runtime.o
	llc-3.6 llvm_test_gc.ll
	clang-3.6 -g -c llvm_test_gc.s
	clang-3.6 -g -I php-src/Zend -o test php-src/Zend/*.o ocaml/byterun/*.o runtime.o llvm_test_gc.o -O0 -lm -ldl -lncurses

semigc:
	cd semigc/ && $(make)

clean:
	rm *.o *.cmi *.cmx
