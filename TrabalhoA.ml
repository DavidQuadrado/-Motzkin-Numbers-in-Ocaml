(*
   
  Disciplina : Programação Funcional
  Aluno : David Quadrado 
  Problema A -> Numeros de Motzkin

*)

(*Função de Motzkin segundo a wikipedia*)

let rec funcao n = (((2 * n + 1)/(n + 2)) * funcao n-1) + (((3 * n -3)/(n + 2))* funcao n-2) 


(*Abrir Bibliotecas*)

open Z

(*Configuração da Biblioteca Zarith*)

let zero   = Z.zero
let one    = Z.one
let two    = Z.succ one
let three  = Z.succ two
let four   = Z.succ three
let ( +> ) = Z.add (* a soma nos inteiros Z *)
let ( *> ) = Z.mul (* a multiplicação nos inteiros Z *)
let ( /> ) = Z.div (* a divisão nos inteiros Z *)
let ( >- ) = Z.sub (* a subtraçao nos inteiros Z*)



(*Tabela de Registo HashTable*)


let a = Hashtbl.create (10001) (*tamanho da tabela*)
let adiciona = Hashtbl.add a (*adiciona um  valor e o respetivo resultado*)
let procura = Hashtbl.find a (*procura se um certo valor já está contido na tabela*)



(*Função Números Motzkin aplicando a Biblioteca Zarith*)

let rec zarmotzkin n = 
  if  n > one then try procura n with Not_found -> let conta = (((two *> n +> one) *> zarmotzkin ( Z.sub n one) +> 
  ((three *> n >- three) *>  zarmotzkin (n >- two)) ) /> (n +> two))  in adiciona n conta ; conta  (* ; ingora os units e vai devolver conta*)
  else one (* segundo a função quando f é 0 ou 1  devolve 1*)
 

(*Leitura dos Inputs e Escrita dos outputs*)

let n = read_int () (*leitura do input*)
let resultadoMotzkin = zarmotzkin (Z.of_int n)


let() = Printf.printf "%s\n" (Z.to_string resultadoMotzkin)  (* tem que ser Z.to_string para que consiga guardar o limite 10000*)

(*Compilar = ocamlfind ocamlopt -o pba TrabalhoA.ml -package zarith -linkpkg *)
