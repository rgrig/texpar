(** Usage: texpar < a.tex > b.tex *)

open Words

let line_length = 65

let typeset ws =
  let wsa = Array.of_list ws in
  let n = Array.length wsa - 1 in
  let b, e = ref 0, ref (-1) in
  while !e < n do begin
    b := !e + 1;
    e := !b;
    while !e < n && String.length wsa.(!e) <= line_length do incr e done;
    let cost = Array.make (!e - !b + 1) (-1,-1) in
    let prev = Array.make (!e - !b + 1) (-1) in
    cost.(0) <- (0,0);
    for i = !b + 1 to !e do begin
      let j = ref i in
      let len = ref (-1) in
      while !j > 0 && !len + String.length wsa.(!j-1) + 1 <= line_length do begin
        decr j;
        len := !len + String.length wsa.(!j) + 1;
        let c = 
          if i = n then
            cost.(!j - !b)
          else
            let c1, c2 = cost.(!j - !b) in
            let c3 = line_length - !len in
            (max c1 c3, c2 + c3 * c3) in
        if cost.(i - !b) = (-1,-1) || cost.(i - !b) > c then begin
          cost.(i - !b) <- c;
          prev.(i - !b) <- !j
        end;
      end done
    end done;
    let rec print_to e =
      if e > !b then begin
        print_to prev.(e - !b);
        for i = prev.(e - !b) to e - 1 do
          print_string wsa.(i);
          if i + 1 = e then
            print_newline ()
          else
            print_char ' '
        done
      end in
    print_to !e;
    print_string wsa.(!e)
  end done

let rec process lexbuf =
  match Words.par lexbuf with
  | [] -> ()
  | p -> typeset p; process lexbuf

let _ =
  let lexbuf = Lexing.from_channel stdin in
  process lexbuf
