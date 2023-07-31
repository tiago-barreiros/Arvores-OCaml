(**
 * @file probD.ml
 * @brief Paz na Terra, comércio justo na galáxia.
 * @details Explorando novos mundos, a humanidade estabeleceu colónias organizadas a volta de buracos
de minhoca em rede com a Terra. O centro desta estrutura de colónias é a Terra, e qualquer
colónia tem um caminho único para a Terra. As colónias trocam matérias primas entre elas e com a Terra, 
via as rotas existentes. Contudo, viajar pelos buracos de minhoca tem o seu custo e cada colónia paga o preço forte
 com as transações. Importa ao governo da Terra perceber que custo cada colónia realmente
tem para suportar o seu comércio.
Importa calcular este custo por planeta.
O objetivo é saber qual o maior custo das viagens
que cada colónia conseguem organizar no contexto das suas relações comerciais.
 * @author Luís Sá, a 46753
   @author Tiago Barreiros, a46118
 **)

(**
 * Com base no enunciado.
 * Definimos o tipo de árvore em que cada nodo guarda a informação do pai.
 **)
type ntree = Node of int * int * (ntree*int) option * (ntree*int) list 

let _ = 
(**
 * Variáveis 'n' e 'm' usadas ao longo do programa.
 * O valor guardado na variável 'n' indica o número de colónias(planetas), fornecido na linha de comandos.
 * O aalor guardado na variável 'm' significa o número de buracos de minhoca(buraco). 
 **)
let planetas = ref 0 in 
planetas := read_int(); 
while !planetas < 0 || !planetas > 10000 do 
  planetas := read_int(); 
done; 
let n = !planetas in 
let buraco = ref 0 in 
buraco := read_int(); 
while !buraco < 0 || !buraco > !planetas do 
  buraco := read_int(); 
done; 
let m = !buraco in 
(**
 * Preenchemos um Array inicial 'buracos' com os dados dos buracos de minhoca.
 * Os dois primeiros valores são as colónias na extremidade do buraco e o ultimo é o custo.
 **)
let buracos = Array.init (m) (fun _ -> Scanf.sscanf (read_line ()) "%d %d %d" (fun x y z -> [|x;y;z|])) in 

(**
 * Função Recursiva 'colonias c buracos n', vai iniciar uma variável 'c', usar o conteúdo do Array 'buracos' e o 
 valor de entrada de 'n'.
 * De seguinda, inicializo um Array auxiliar vazio para guardar as informações obtidas enquanto precorro o Array 'buracos'
 em busca dos filhos.
 **)
let rec colonias c buracos n = 
  let aux = Array.make n ((Node (0,0,None,[])), 0) in 
  let j = ref 0 in 
  match c with  
 |Node (a,_,_,_) -> (for i=0 to (Array.length(buracos) - 1) do if (buracos.(i).(0) = a) then aux.(!j) <- 
                    ((colonias (Node (buracos.(i).(1),0, Some (c,buracos.(i).(2)),[])) buracos n), buracos.(i).(2)); j := !j + 1 done);

 let rec lista n aux l = 
 if (n > -1) then match aux.(n) with 
                  |(_,o) -> if (o <> 0) then lista (n-1) aux (aux.(n)::l) else lista (n-1) aux l
 else l in

 let filhos = lista (n-1) (aux) ([]) in

 let rec cima c2 = 
  match c2 with
  |Node (_,_,p,_) -> (match p with
                     |None -> 0 
                     |Some (r,s) -> s + cima r) in

 let rec baixo filhos valor =
  match filhos with
  |cabeca::cauda -> (match cabeca with 
                    |(filho,custo) -> (match filho with 
                                      |Node (_,_,_,k) -> let valor_subarvore = baixo k (valor + custo) in
                                      let outros_filhos = baixo cauda valor in
                                      if (valor_subarvore > outros_filhos) then valor_subarvore else outros_filhos))
  |_ -> valor in

  let custo_cima = cima c in

  let custo_baixo = baixo filhos 0 in

  let custo_maximo = if (custo_cima > custo_baixo) then custo_cima else custo_baixo in
  
  match c with
  |Node (r,_,z,_) -> Node (r,custo_maximo,z,filhos) 
  
in

let arvore = colonias (Node(1,0,None,[])) buracos n in 

let rec custos_arvore arvore arr = 
  match arvore with 
  |Node (t,r,_,q) -> (arr.(t-1) <- r;
                     (let rec custos_filhos l arr = 
                        match l with 
                        |cabeca::cauda -> (match cabeca with
                                          |(y,_) -> custos_arvore y arr; custos_filhos cauda arr) 
                        |[] -> print_string "" in
                        custos_filhos q arr))
                      
in 

let arr_custos = Array.make n 0 in
custos_arvore arvore arr_custos;
for i = 0 to (n-1) do Printf.printf "%d\n" arr_custos.(i) done;;