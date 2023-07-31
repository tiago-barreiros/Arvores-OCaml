(*Lista Vazia, definir um nó, cabeça e cauda*)
type lista = V | C of int * lista (*Lista de inteiros*)

(*EXEMPLO DE LISTA*)
(*3(à cabeça) --> 8 --> 5 --> ||(vazia)*)
let li = C (3,(C (8, (C (5,V)))))

(*Lista de qualquer tipo*)
type 'a lista = V | C of 'a * 'a lista

(*Listas ligadas para poderem ser alteradas*)
                       (*é uma estrutura*)
type 'a listl = 'a lista_ligada ref (*tem o conteúdo e tem o next*)
  and 'a lista_ligada = {mutable contents: 'a; next: 'a listl}

(*definir duas funções ao mesmo tempo*)
(*
let rec f x = ... g ...
and g x = ... f ...
*)

(*Definir listas numeradas*)
type dia = Seg | Ter | Qua | Qui | Sex | Sab | Dom
(*Perceber a potência do match*)
let dia_util = function (*é igual, apenas uma questão de estilo*)
  | Dom | Sab -> false
  | _         -> true

(*Quero saber se uma lista tem exatamente 2 elementos*)
(*exaustivo, a ordem é importante, o primeiro ganha*)
let tem_duas_linhas l = (*function*)
  match l with
  | [_;_] (*_::_::[]*) -> true
  | _ -> false

let tem_duas = function [_;_] -> true | _ -> false

(*O Ocaml não tem o valor nulo com em C (NULL)*)
(*Uma ÀRVORE pode ser vazia*)
(*
Uma árvore é uma folha ou
uma árvore de raiz x (inteiro) e de filhos esq e dir

    F  ou        x:int
                /     \
              esq     dir
*)

type arvore = F | N of arvore * int * arvore

(*EXEMPLO
        5
     4     7
   F   F F   8
            F  F
*)
let ex = N ((N (F,4,F)),5,N (F,7,N(F,8,F)))

(*ÁRVORES ternárias*)
type arv = F | N of int * arv * arv * arv

(*Árvore qualquer numero de filhos*)
                  (*na raiz há um inteiro e os filhos vai uma lista deles*)
type arv = F | N of int * arv list

(*Árvore binária de qualquer tipo*)
type 'a arv = F | N of 'a arv * 'a * 'a arv

(*Como listar os elementos que estão numa árvore*)
(*
previligiando a profundidade de uma escolha em vez da largura

Depth First Search - DFS(Profundidade primeiro)
Breadth First Search -  BFS(Largura primeiro)
*)
(*
Função vai correr uma árvore e vai colocando numa lista os valores que encontra
*)
let rec to_list a(*árvore a*) =
  match a with
  | F             -> []
  | N (esq,x,dir) -> to_list esq @(*concateno*) x :: to_list dir  (*ser um nodo que tem uma árvore esquerda, um x que está na raiz e uma árvore direita*)

(*chamar a função
let l = to_list ex;;
*) (*é uma árvore binária de pesquisa, pois o valor que está à direita é sempre maior*)





