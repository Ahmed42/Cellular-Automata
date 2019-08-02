type cell = Alive | Dead

let next_world_state world rule =
	let next_cell_state neighborhood rule =
		match neighborhood with
		| (Dead, Dead, Dead)
		| (Alive, Dead, Dead)
		| (Alive, Alive, Alive) -> Dead
		| _ -> Alive 
	in
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
			print_char ' ';
			pretty_print world

let rec run_world world rule steps=
	match steps with
	| 0 -> print_endline "Done!"
	| n -> 
		pretty_print world; 
		run_world (next_world_state world rule) rule (steps - 1) 
