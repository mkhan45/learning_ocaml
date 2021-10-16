type operator =
    | Add
    | Sub
    | Mul
    | Div;;

type token =
    | Number of float
    | Ident of string
    | Operator of operator
    | LParen
    | RParen;;

let string_chars s =
    List.init (String.length s) (String.get s);;

module type Scanner_type = sig
    val scan : string -> token list
end

module Scanner : Scanner_type = struct
    let is_digit d = '0' <= d && d <= '9';;
    let is_numeric d = is_digit d || d == '.';;

    let rec string_of_chars chars = 
        let buf = Buffer.create 16 
            in List.iter (Buffer.add_char buf) chars;
        Buffer.contents buf
    
    and scan_digit ls =
        let rec aux ls acc = match ls with
            | d::xs when is_numeric d -> aux xs (d::acc)
            | _ -> let f = (acc |> List.rev |> string_of_chars |> float_of_string)
                    in (Number f)::(scan_ls ls)
        in aux ls []

    and scan_ls = function
        | [] -> []
        | ' '::xs -> scan_ls xs
        | '\t'::xs -> scan_ls xs
        | '+'::xs -> Operator Add :: (scan_ls xs)
        | '-'::xs -> Operator Sub :: scan_ls xs
        | '*'::xs -> Operator Mul :: scan_ls xs
        | '/'::xs -> Operator Div :: scan_ls xs
        | '('::xs -> LParen :: scan_ls xs
        | ')'::xs -> RParen :: scan_ls xs
        | d::_ as ls when is_digit d -> scan_digit ls
        | _ -> assert false;;

    let scan s = s |> string_chars |> scan_ls;;
end

type expr =
    | Atomic of float
    | Binary of {lhs: expr; op: operator; rhs: expr};;

let rec eval_expr = function
    | Atomic n -> n
    | Binary ({op = Add} as e) -> (eval_expr e.lhs) +. (eval_expr e.rhs)
    | Binary ({op = Sub} as e) -> (eval_expr e.lhs) -. (eval_expr e.rhs)
    | Binary ({op = Mul} as e) -> (eval_expr e.lhs) *. (eval_expr e.rhs)
    | Binary ({op = Div} as e) -> (eval_expr e.lhs) /. (eval_expr e.rhs)

module type Parser_type = sig
    val parse : string -> expr
end

module Parser : Parser_type = struct
    let op_bp = function
        | Add | Sub -> (4, 5)
        | Mul | Div -> (6, 7);;
    
    let rec complete_expr lhs ls min_bp = match ls with
        | (Operator op)::xs ->
                let (l_bp, r_bp) = op_bp op
                in
                if l_bp < min_bp 
                    then (lhs, ls)
                    else let (rhs, rem) = expr_bp xs r_bp in 
                         let complete = Binary {op = op; lhs = lhs; rhs = rhs}
                          in complete_expr complete rem min_bp
        | _ -> (lhs, ls)

    and expr_bp ls min_bp = match ls with
        | (LParen::xs) ->
                let (paren_expr, temp) = expr_bp xs 0
                in
                if temp == [] || List.hd temp != RParen
                    then assert false
                    else complete_expr paren_expr (List.tl temp) min_bp
        | (Number f)::xs -> complete_expr (Atomic f) xs min_bp
        | _ -> assert false;;

    let parse s = let scanned = Scanner.scan s in 
                  let (expr, _) = expr_bp scanned 0
                  in expr;;
end

let eval s = s |> Parser.parse |> eval_expr;;
