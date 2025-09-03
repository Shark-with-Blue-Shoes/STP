open STP.Lexer

let read_file file : string = In_channel.with_open_bin file In_channel.input_all;;

let interp file = 
  print_string "interpreting file...\n";
  let str = read_file file in
    let lex = new lexer str in
      let _ = lex#tokenize in
      ();;

let () = interp "test.stp";;
