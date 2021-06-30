open Myinterp

let srcfile = ref "-"
let outfile = ref "-"
let debug = ref false
let dprint s = if !debug then (print_string (s ()) ; flush stdout)
let help = "Help: " ^ Sys.argv.(0) ^ " [-o ofile] [file]"

let arg_spec = Arg.align [
    ("-o", Arg.Set_string outfile,
     " Path to the output file (default: stdout)");
  ]

let compile prompt ichan cont =
  print_string prompt;
  flush stdout;
  let lexbuf = Lexing.from_channel ichan in
  let prog = Parser.program Lexer.tokenize lexbuf in
  Eval.print_program Env.empty prog;
  cont ()

let _ =
  Arg.parse arg_spec (fun s -> srcfile := s) help;
  if !srcfile = "-" then
    let c = stdin in
    let rec k () = compile "> " c k in
    compile "> " c k
  else
    let c = open_in !srcfile in
    let k () = close_in c in
    compile "" c k