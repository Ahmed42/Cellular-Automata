type cell = Alive | Dead
(* type world = cell list *) 

let rules = [(Alive, Dead, Alive); (Dead, Dead, Dead); (Alive, Alive, Alive)]

let rule_110 = [(Alive, Alive, Dead); (Alive, Dead, Alive); (Dead, Alive, Alive); (Dead, Alive, Dead); (Dead, Dead, Alive)]

let rec next_cell_state neighborhood rules =
	match rules with
	| [] -> Dead
	| (l, m, r) :: rules -> 
		match neighborhood with
		| (left, middle, right) ->
			if l = left && m = middle && r = right 
			then Alive 
			else next_cell_state (left, middle, right) rules


let next_world_state world rule =
	(*let next_cell_state neighborhood rule =
		match neighborhood with
		| (Dead, Dead, Dead)
		| (Alive, Dead, Dead)
		| (Alive, Alive, Alive) -> Dead
		| _ -> Alive 
	in*)
	match world with
	| [] -> []
	| left :: [] -> [next_cell_state (Dead, left, Dead) rule]
	| left :: middle :: rest ->
		let rec next_world_state world rule =
			match world with
			| left :: [] -> []
			| left :: [right] -> [next_cell_state (left, right, Dead) rule]
			| left :: middle :: right :: rest -> 
				(next_cell_state (left, middle, right) rule) :: (next_world_state (middle :: right :: rest) rule)
		in
		(next_cell_state (Dead, left, middle) rule) :: (next_world_state (left :: middle :: rest) rule)



let rec pretty_print world =
	match world with
	| [] -> print_newline ()
	| cell :: world -> 
			print_char (if cell = Alive then '#' else '_'); 
			(*print_char ' ';*)
			pretty_print world

let rec run_world world rule steps=
	match steps with
	| 0 -> print_endline "Done!"
	| n -> 
		pretty_print world; 
                run_world (next_world_state world rule) rule (steps - 1);; 

(*run_world [Alive; Dead; Alive; Alive; Alive; Dead; Dead] rule_110 10 *)


let rec build_list value n=
        if n = 0
        then []
        else value :: (build_list value (n - 1));;


let decode_rule rule_num=
        let rec dec2bin num= 
                match num with
                | 0 -> []
                | _ -> (num mod 2) :: (dec2bin (num/2)) in
        let bit2cell bit=
                match bit with
                | 0 -> Dead
                | 1 -> Alive in
        let rec bin2morebin rule_bin bit_num=
                match rule_bin with
                | [] -> []  
                | bit :: rule_bin ->
                                if bit = 0
                                then (bin2morebin rule_bin (bit_num + 1)) 
                                else (List.rev (dec2bin bit_num)) :: (bin2morebin rule_bin (bit_num + 1)) in
        let rec bin2cells rule_bin=
                match rule_bin with
                | [] -> [] 
                | [bit0] :: rule_bin -> (Dead, Dead, bit2cell bit0) :: (bin2cells rule_bin) 
                | [bit1; bit0] :: rule_bin -> (Dead, bit2cell bit1, bit2cell bit0) :: (bin2cells rule_bin) 
                | [bit2; bit1; bit0] :: rule_bin -> (bit2cell bit2, bit2cell bit1, bit2cell bit0) :: (bin2cells rule_bin) in

        bin2cells (bin2morebin (dec2bin rule_num) 0);;


(*run_world [Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Dead; Alive] rule_110 100*)
(*run_world ((build_list Dead 99)@[Alive]) rule_110 100*)

(*let dec2bin num=
        let rec dec2bin_rev n=
                match n with
                | 0 -> []
                | _ -> (n mod 2) :: (dec2bin_rev (n/2))
        in
                List.rev (dec2bin_rev num);;*)

let print_cell cell=
        match cell with
        | Alive -> print_char 'A'
        | Dead -> print_char 'D';;

let print_rule rule=
        List.map (fun pat -> match pat with (c2, c1, c0) -> print_cell c2; print_cell c1; print_cell c0; print_char ' ') rule;;



run_world ((build_list Dead 199)@[Alive]) (decode_rule 90) 500
