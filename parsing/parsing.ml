include Parsetree

let pexp_from_string string = Lexer.from_string Parser.pexp_opt string
let ptyp_from_string string = Lexer.from_string Parser.ptyp_opt string
