(* ========== Vaja 2: Uvod v funkcijsko programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Vektorje predstavimo kot seznam števil s plavajočo vejico.
[*----------------------------------------------------------------------------*)

type vector = float list

(*----------------------------------------------------------------------------*]
Definirajte enotske vektorje `i`, `j` in `k` v treh dimenzijah.
[*----------------------------------------------------------------------------*)

let i: vector = [1.0;0.0;0.0]
let j: vector = [0.0;1.0;0.0]
let k: vector = [0.0;0.0;1.0]

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razteg : float -> vector -> vector`, ki vektor, 
predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

let rec razteg vektor skalar = 
    List.map (( *. ) skalar) vektor

(*----------------------------------------------------------------------------*]
Napišite funkcijo `sestej : vector -> vector -> vector`, ki vrne vsoto dveh 
vektorjev.
[*----------------------------------------------------------------------------*)

let rec sestej vekt1 vekt2 = 
    match vekt1, vekt2 with                                                     
        | [], _ -> vekt1
        | _, [] -> vekt2
        | x1 :: tl1 , x2 :: tl2 -> (x1 +. x2) :: (sestej tl1 tl2)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `skalarni_produkt : vector -> vector -> float`, ki izračuna 
skalarni produkt dveh vektorjev
[*----------------------------------------------------------------------------*)

let rec skalarni_produkt vekt1 vekt2= 
    List.combine vekt1 vekt2
    |> List.map (fun (x1, x2) -> x1 *. x2)
    |> List.fold_left ( +. ) 0.


(*----------------------------------------------------------------------------*]
Napišite funkcijo `norma : vector -> float`, ki vrne evklidsko normo vektorja.
[*----------------------------------------------------------------------------*)

let rec norma vektor = 
    Float.sqrt ( skalarni_produkt vektor vektor )

(*----------------------------------------------------------------------------*]
Napišite funkcijo `projeciraj : vector -> vector -> vector`, ki izračuna 
projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let rec projeciraj vekt1 vekt2 = 
    let skalar = ( skalarni_produkt vekt1 vekt2 ) /.  ( norma vekt2 ) in
    razteg vekt2 skalar

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML 
oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.

Primer:
`ovij "h1" "Hello, world!"`

[*----------------------------------------------------------------------------*)

let rec ovij znacka vsebina = "<" ^ znacka ^ ">" ^ vsebina ^ "<" ^ "/" ^ znacka ^ ">"  

(*----------------------------------------------------------------------------*]
Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število 
presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za ustrezno število presledkov.

Primer:
`zamakni 4 "Hello, world!"`

[*----------------------------------------------------------------------------*)

let rec zamakni n niz = 
    String.split_on_char '\n' niz
    |> List.map ( fun s -> (String.make n ' ') ^ s )
    |> String.concat "\n"

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne 
niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:

Primer:
`ul ["ananas"; "banana"; "čokolada"]`

[*----------------------------------------------------------------------------*)

let rec ul sez = 
    List.map ( ovij "li" ) sez
    |> String.concat "\n"
    |> zamakni 2 
    |> (fun s -> "\n" ^ s ^ "\n")
    |> ovij "ul" 

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme niz, 
ki vsebuje vejico, loči na del pred in del za njo.

Primer:
`razdeli_vrstico "mleko, 2"`

[*----------------------------------------------------------------------------*)

let rec razdeli_vrstico niz = 
    let sep = ',' in
    let sez = String.split_on_char sep niz in
    match sez with
        | a :: b :: _ -> (a, b)
        | _ -> failwith "Ne"

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`, 
ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike 
"izdelek, vrednost", in vrne seznam ustreznih parov.

Primer:
`pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"`

[*----------------------------------------------------------------------------*)

let rec pretvori_v_seznam_parov niz = 
    let sez_nizov = String.split_on_char '\n' niz in
    List.map (razdeli_vrstico) sez_nizov

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list -> (string * 'b) list`,
ki dano funkcijo uporabi na vseh drugih komponentah elementov seznama.

Primer:
```ml
let seznam = [("ata", "mama"); ("teta", "stric")] in 
pretvori_druge_komponente String.length seznam
```

[*----------------------------------------------------------------------------*)

let rec pretvori_druge_komponente f sez = 
    List.map ( fun (a,b) -> (a, f b)) sez

(*----------------------------------------------------------------------------*]
Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki 
sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni 
znesek nakupa.

Primer:
```ml
let nakupovalni_seznam = "mleko, 2\njabolka, 5"
and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
izracunaj_skupni_znesek cenik nakupovalni_seznam
```

[*----------------------------------------------------------------------------*)

let rec izracunaj_skupni_znesek niz =
    pretvori_v_seznam_parov niz 
    |> pretvori_druge_komponente float_of_string
    |> List.map (fun (a,b) -> b)
    |> List.fold_left ( +. ) 0.
