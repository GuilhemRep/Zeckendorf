let nfibo=50;;

(* Suite de Fibonacci *)
let f =
  let k = Array.make nfibo 0
  in
    k.(1)<-1;
    for i=2 to (nfibo-1) do
      k.(i)<-k.(i-1) + k.(i-2)
    done;
    k;;

(* Terme maximal de f inferieur a n *)
let maxmin n t =
  let r=ref 0 in
    for i=0 to (Array.length t -1) do
      if t.(i)<=n then
        r:= t.(i)
    done;
    !r;;

(* Decomposition de Zeckendorf de n *)
let rec zeck n =
  if n=0 then []
  else if n<=f.(1) then [n]
  else let k = maxmin n f in
      k::(zeck (n-k));;

(* Renvoie l'indice de la case de cartes qui contient i *)
let indice i cartes =
  let rec aux a b =
    if b<a then failwith "erreur"
    else if a=b then
      if fst (cartes.(a))<>i then failwith "pas de carte"
      else a
    else let m=(b+a)/2 in
      let ind = fst (cartes.(m)) in
        if ind > i then aux a m
        else if ind=i then m
        else aux m b
  in
    aux 0 (Array.length cartes - 1);;

(* Ajoute n aux cartes listees dans l *)
let rec ajoute_nombre_aux_cartes n l cartes =
  match l with
      []->()
    |t::q->
        let i=indice t cartes in
          cartes.(i)<- (fst (cartes.(i)), n :: snd (cartes.(i)));
          ajoute_nombre_aux_cartes n q cartes;;

(* Cree les cartes : tableau de la forme (premier_nombre, [liste des nombres]) *)
let cartes = let t = Array.make (nfibo-2) (0,[]) in
    for i=2 to (nfibo-1) do
      t.(i-2)<- (f.(i),[])
    done;
    t;;

(* Genere les cartes pour 200 nombres *)
for i=0 to 200 do
  ajoute_nombre_aux_cartes i (zeck i) cartes
done;;

List.rev (snd (cartes.(0)));;
