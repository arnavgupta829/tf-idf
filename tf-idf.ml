let format_line line =
	let format = ref "" in  
		for i = 0 to (String.length line -1) do 
			if (line.[i] = '.' || line.[i] = ',') then 
				format := (!format)^" "
			else 
				format := (!format)^(String.make 1 line.[i])
	done; String.lowercase !format

let rec sep s =
	if String.contains s (' ') then 
		(String.sub s 0 (String.index s ' '))::sep (String.sub s ((String.index s ' ')+1) ((String.length s)-((String.index s ' ')+1)))
	else [s]

let rec parsefile in_channel doc_list = 
	try
		let s = input_line in_channel in 
			parsefile in_channel (s::doc_list)
	with End_of_file-> doc_list

let read_filedoc in_file = 
	let in_channel = open_in in_file in 
		List.rev (parsefile in_channel [])

let tf word line = 
	let counter = ref 0 in 
		for i = 0 to (List.length line -1) do 
			if (List.nth line i = word) then 
				counter := !counter + 1
			else 
				()
	done; (float (!counter) /. float (List.length line))

let idf word doc_list = 
	let counter = ref 0 in 
		for i = 0 to (List.length doc_list -1) do
			if (List.mem word (sep (List.nth doc_list i))) then 
				counter := !counter + 1
			else 
				()
	done; log10 (float (List.length doc_list) /. float (!counter))

let give_pos word line_list = 
	let pos_list = ref [] in
		for i = 0 to (List.length line_list -1) do
			if (List.nth line_list i = word) then
				pos_list := i::(!pos_list)
			else
				()
	done; List.rev (!pos_list)

let give_triplet word doc_list = 
	let out_list = ref [] in
		for i = 0 to (List.length doc_list -1) do
			if (List.mem word (sep (List.nth doc_list i))) then
				out_list := [(i , (tf word (sep (List.nth doc_list i))) , (give_pos word (sep (List.nth doc_list i))))]@(!out_list)
			else
				()
	done; List.rev (!out_list)

let rec remove l = 
	match l with
	| hd::tl -> if (List.mem hd tl) then remove tl else hd::(remove tl)
	| [] -> []

let give_words doc_list = 
	let lst = ref [] in 
		for i = 0 to (List.length doc_list -1) do 
			lst := (sep (List.nth doc_list i))@(!lst)
	done; List.sort compare (remove !lst)
	
let inverted_index doc_list = 
	let word_list = give_words doc_list and out_list = ref [] in 
		for i = 0 to (List.length word_list -1) do
			out_list := [((List.nth word_list i) , (idf (List.nth word_list i) doc_list) , (give_triplet (List.nth word_list i) doc_list))]@(!out_list)
	done; List.rev (!out_list)

let main in_file = 
	let doc_list = read_filedoc in_file in 
		inverted_index doc_list
	
