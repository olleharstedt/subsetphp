subsetphp: realpath.o hh_shared.o parser_hack.cmx main.ml
	ocamlfind ocamlopt -package ppx_deriving.show -linkall ident.cmx utils.cmx unix.cmxa str.cmxa sys_utils.cmx path.cmx relative_path.cmx pos.cmx errors.cmx lexer_hack.cmx namespace_env.cmx lint.cmx prefix.cmx eventLogger.cmx realpath.o hh_shared.o sharedMem.cmx parser_heap.cmx namespaces.cmx parser_hack.cmx fileInfo.cmx ast.cmx main.ml -o subsetphp

parser_hack.cmx: lint.cmx lexer_hack.cmx parser_heap.cmx ast.cmx namespaces.cmx parser_hack.cmi parser_hack.ml
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

clean:
	rm *.o *.cmi *.cmx
