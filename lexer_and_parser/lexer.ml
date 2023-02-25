open TokenTypes
open Str

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)
(*fix_wsl2_interop *)
let tokenize input = 
    let length = String.length input in
    let rec tok pos = 
        if pos >= length then []
        else if string_match (Str.regexp "if") input pos then
        Tok_If::(tok (pos + 2))
        else if string_match (Str.regexp "then") input pos then
        Tok_Then::(tok (pos + 4))
        else if string_match (Str.regexp "else") input pos then
        Tok_Else::(tok (pos + 4))
        else if string_match (Str.regexp "let") input pos then
        Tok_Let::(tok (pos + 3))
        else if string_match (Str.regexp "def") input pos then
        Tok_Def::(tok (pos + 3))
        else if string_match (Str.regexp "in") input pos then
        Tok_In::(tok (pos + 2))
        else if string_match (Str.regexp "rec") input pos then
        Tok_Rec::(tok (pos + 3))
        else if string_match (Str.regexp "fun") input pos then
        Tok_Fun::(tok (pos + 3))
        else if string_match (Str.regexp "not") input pos then
        Tok_Not::(tok (pos + 3))

        else if string_match (Str.regexp "true\\|false") input pos then
            if matched_string input = "true" then
                Tok_Bool(true)::(tok (pos + 4))
            else Tok_Bool(false)::(tok (pos + 5))

        else if string_match (Str.regexp "\"[^\"]*\"") input pos then
        let placeholder = matched_string input in 
        let length = String.length placeholder in
        Tok_String(String.sub placeholder 1 (length - 2))::(tok (pos + String.length placeholder))


        else if string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then
        let placeholder = matched_string input in
        Tok_ID(placeholder)::(tok (pos + String.length placeholder))

        else if string_match (Str.regexp "[0-9]+") input pos then
        let value = matched_string input in 
        Tok_Int(int_of_string value)::(tok (pos + String.length value))


        else if string_match (Str.regexp "\\((-\\)+[0-9]+)+") input pos then
            (if string_match (Str.regexp "-[0-9]+") input (search_forward (Str.regexp "-[0-9]+") input 0) then
            let value = matched_string input in 
            Tok_Int(int_of_string value)::(tok (pos + String.length (input)))
            else failwith "error")
        
        else if string_match (Str.regexp ">=") input pos then
        Tok_GreaterEqual::(tok (pos + 2))
        else if string_match (Str.regexp "<=") input pos then
        Tok_LessEqual::(tok (pos + 2))
        else if string_match (Str.regexp "<>") input pos then
        Tok_NotEqual::(tok (pos + 2))
        else if string_match (Str.regexp "->") input pos then
        Tok_Arrow::(tok (pos + 2))
        else if string_match (Str.regexp ">") input pos then
        Tok_Greater::(tok (pos + 1))
        else if string_match (Str.regexp "<") input pos then
        Tok_Less::(tok (pos + 1))
        else if string_match (Str.regexp "=") input pos then
        Tok_Equal::(tok (pos + 1))
        else if string_match (Str.regexp ";;") input pos then
        Tok_DoubleSemi::(tok (pos + 2))
        else if string_match (Str.regexp "||") input pos then
        Tok_Or::(tok (pos + 2))
        else if string_match (Str.regexp "&&") input pos then
        Tok_And::(tok (pos + 2))
        else if string_match (Str.regexp "\\^") input pos then
        Tok_Concat::(tok (pos + 1))

        else if string_match (Str.regexp "(") input pos then 
        Tok_LParen::(tok (pos + 1))
        else if string_match (Str.regexp ")") input pos then 
        Tok_RParen::(tok (pos + 1))
        else if string_match (Str.regexp "\\+") input pos then
        Tok_Add::(tok (pos + 1))
        else if string_match (Str.regexp "\\-") input pos then
        Tok_Sub::(tok (pos + 1))
        else if string_match (Str.regexp "\\*") input pos then
        Tok_Mult::(tok (pos + 1))
        else if string_match (Str.regexp "\\/") input pos then
        Tok_Div::(tok (pos + 1))
        else tok (pos + 1)
    in tok 0