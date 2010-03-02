(* TODO(rg): Does LaTeX ignore leading and trailing spaces in the argument of begin/end? *)
(* TODO(rg): $...$ and $$...$$ and \[...\] and \(...\) *)
(* TODO(rg): environments should be configurable. perhaps even parse newtheorem *)

{
  exception Unpaired_environment of string * string
  exception Eof_in_environment of string

  let text_environment = Hashtbl.create 13
  let _ = List.iter (fun e -> Hashtbl.add text_environment e ()) [
    "corollary";
    "definition";
    "document";
    "example";
    "lemma";
    "problem";
    "proof";
    "proposition";
    "remark";
    "theorem"]
}

rule par = parse
  | [' ' '\t' '\n']+ as s { [s] }
  | ((['A'-'Z' 'a'-'z' '0'-'9'] [^' ' '\t' '\n']*) | "\\noindent" | "\\indent") as w {
      par_continue [w] lexbuf 
    }
  | "\\begin{" ([^ '}']* as e) '}' [' ' '\t' '\n']* as s {
      if Hashtbl.mem text_environment e then [s]
      else begin
        let b = Buffer.create 1000 in (* TODO(rg): choose based on stats *)
        Buffer.add_string b s;
        [verbatim true b 1 lexbuf]
      end
    }
  | '%' [^'\n']* '\n'+ as s { [s] }
  | eof { [] }
  | _ as c { 
      let b = Buffer.create 1000 in
      Buffer.add_char b c;
      [verbatim (c = '{') b (if c = '{' then 1 else 0) lexbuf]
    }
and par_continue accumulator = parse
  | "\\begin{" ([^'}']* as e) "}" [' ' '\t' '\n']* as w {
      if Hashtbl.mem text_environment e then 
        List.rev (w :: accumulator)
      else begin
        let b = Buffer.create 1000 in (* TODO(rg): choose based on stats *)
        Buffer.add_string b w;
        List.rev (verbatim true b 1 lexbuf :: accumulator)
      end
    }
  | "\\end{" [^ '}']* '}' [' ' '\t' '\n']* as s { List.rev (s :: accumulator) }
  | eof { List.rev ("" :: accumulator) }
  | '%' [^ '\n']* '\n'+ as s
  | '\n' (['\t' ' ']* '\n'+ as s) { List.rev (s :: accumulator) }
  | [^ '\t' ' ' '\n']+ as w {
	    par_continue (w :: accumulator) lexbuf 
	  }
  | ['\t' ' ' '\n']+ { par_continue accumulator lexbuf }
and verbatim mode buffer cnt = parse
  | (("\\begin{" [^'}']* '}') | "{") as s {
      Buffer.add_string buffer s;
      verbatim mode buffer (cnt + 1) lexbuf
    }
  | (("\\end{" [^'}']* '}') | "}") as s {
      Buffer.add_string buffer s;
      if mode && cnt <= 1 then
        whitespace buffer lexbuf
      else
        verbatim mode buffer (cnt - 1) lexbuf
    }
  | '\n' [' ' '\t']* '\n'+ as s {
      Buffer.add_string buffer s;
      if cnt <= 0 then 
        Buffer.contents buffer 
      else 
        verbatim mode buffer cnt lexbuf
    }
  | '%' [^'\n']* as s { (* skip commented groups *)
      Buffer.add_string buffer s;
      verbatim mode buffer cnt lexbuf
    }
  | eof { Buffer.contents buffer }
  | _ as c {
      Buffer.add_char buffer c;
      verbatim mode buffer cnt lexbuf
    }
and whitespace buffer = parse
  | ['\n' ' ' '\t']* as s { 
      Buffer.add_string buffer s;
      Buffer.contents buffer
    }
