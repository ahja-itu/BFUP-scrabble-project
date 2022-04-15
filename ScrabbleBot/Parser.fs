// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser      // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"
    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"
    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel    = pstring "isVowel"
    let pif         = pstring "if"
    let pthen       = pstring "then"
    let pelse       = pstring "else"
    let pwhile      = pstring "while"
    let pdo         = pstring "do"
    let pdeclare    = pstring "declare"

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.2
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many1 whitespaceChar <?> "spaces1"

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.3
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.4
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    let parseAnyBrackets p bracketOpen bracketClose
        = pchar bracketOpen >*>. p .>*> pchar bracketClose
    let parenthesise p = parseAnyBrackets p '(' ')'
    let curlybracketise p = parseAnyBrackets p '{' '}'
    
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.5
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    let charListToString lst = lst |> List.map string |> List.reduce (+)

    let pid =
        (pchar '_' <|> pletter) .>>.
        many (palphanumeric <|> pchar '_') |>> 
        (fun (x, y) -> charListToString (x :: y)) <?> "id"

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.6
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    let unop op = fun a -> op >*>. a

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.7
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    let binop (op: Parser<'a>) (p1: Parser<'b>) (p2: Parser<'c>) : Parser<'b * 'c>
        = p1 .>*> op .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    
    // For exercise 7.9
    let charParse, cref = createParserForwardedToRef<cExp>()

    // For exercise 7.10
    let BoolParse1, bp1ref = createParserForwardedToRef<bExp>()
    let BoolParse2, bp2ref = createParserForwardedToRef<bExp>()
    let BoolParse3, bp3ref = createParserForwardedToRef<bExp>()

    // For exercise 7.11
    let StmntParsePrimary, spref = createParserForwardedToRef<stm>()
    let StmntParseSecondary, ssref = createParserForwardedToRef<stm>()

    // Given from the template
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.8
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    let AddParse = binop (pchar '+') ProdParse ProdParse   |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse ProdParse   |>> Sub <?> "Sub"
    let DivParse = binop (pchar '/') AtomParse ProdParse   |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse   |>> Mod <?> "Mod"
    let PVParse  = pPointValue >*>. parenthesise ProdParse |>> PV  <?> "PV" //
    let NegParse = unop (pchar '-') AtomParse              |>> (fun f -> Mul ((N -1), f)) <?> "Neg"
    let VParse   = pid |>> V <?> "V"

    // From the future
    let CharToIntParse = pCharToInt >*>. parenthesise charParse |>> CharToInt <?> "CharToInt"

    // Given
    let AexpParse = TermParse 

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.9
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    let CharParse      = pchar '\''  >>. anyChar .>> pchar '\'' |>> C         <?> "C"
    let CharValueParse = pCharValue >*>. parenthesise AexpParse |>> CV        <?> "CV" 
    let IntToCharParse = pIntToChar >*>. parenthesise AexpParse |>> IntToChar <?> "IntToChar"
    let ToUpperParse   = pToUpper   >*>. parenthesise charParse |>> ToUpper   <?> "ToUpper"
    let ToLowerParse   = pToLower   >*>. parenthesise charParse |>> ToLower   <?> "ToLower"

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.8 + 7.9
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    // Term Parsing
    do tref.Value <-
        choice [ 
            AddParse
            SubParse
            ProdParse
        ]

    // Product Parsing
    do pref.Value <-
        choice [ 
            MulParse
            DivParse
            ModParse
            AtomParse
        ]

    // Atom Parsing
    do aref.Value <-
        choice [
            ParParse
            NegParse
            PVParse
            CharToIntParse
            VParse
            NParse
        ]

    do cref.Value <- 
        choice [
            IntToCharParse
            CharValueParse
            CharParse
            ToUpperParse
            ToLowerParse
        ]

    let CexpParse = charParse

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.10
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


    let TrueParse = pTrue |>> (fun _ -> TT) <?> "TT"
    let FalseParse = pFalse |>> (fun _ -> FF) <?> "FF"

    // Loosest bindings
    let ConjunctionParse = binop (pstring "/\\") (BoolParse2) (BoolParse1) |>> Conj <?> "Conj"
    let DisjunctionParse = binop (pstring "\\/") (BoolParse2) (BoolParse1) |>> (fun (l, r) -> Conj ((Not l), (Not r))) <?> "Disj"
    
    // Middle-strength bindings
    let EqualParse          = binop (pchar '=')    AexpParse AexpParse |>> AEq <?> "="
    let DifferentParse      = binop (pstring "<>") AexpParse AexpParse |>> (fun (l, r) -> l .<>. r) <?> "<>"
    let LessThanParse       = binop (pchar '<')    AexpParse AexpParse |>> ALt <?> "<"
    let LessThanEqualsParse = binop (pstring "<=") AexpParse AexpParse |>> (fun (l, r) -> l .<=. r) <?> "<="
    let GreaterThan         = binop (pchar '>')    AexpParse AexpParse |>> (fun (l, r) -> l .>. r) <?> ">"
    let GreaterEqualsThan   = binop (pstring ">=") AexpParse AexpParse |>> (fun (l, r) -> l .>=. r) <?> ">="

    // Strongest bindings
    let NotParse      = unop (pchar '~') BoolParse1           |>> Not      <?> "Not"
    let IsLetterParse = pIsLetter >*>. parenthesise charParse |>> IsLetter <?> "IsLetter"
    let IsDigitParse  = pIsDigit  >*>. parenthesise charParse |>> IsDigit  <?> "IsDigit"
    let IsVowelParse  = pIsVowel  >*>. parenthesise charParse |>> IsVowel  <?> "IsVowel"

    // TODO: Nuværende implementation kan ikke håndtere parenteser der grupperer statements sammen

    do bp1ref.Value <-
        choice [
            ConjunctionParse
            DisjunctionParse
            BoolParse2
        ]

    do bp2ref.Value <-
        choice [
            EqualParse
            DifferentParse
            LessThanParse
            LessThanEqualsParse
            GreaterThan
            GreaterEqualsThan
            BoolParse3
        ]

    do bp3ref.Value <-
        choice [
            NotParse
            IsLetterParse
            IsDigitParse
            IsVowelParse
            FalseParse
            TrueParse
        ]

    let BexpParse = BoolParse1

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.11
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    let LetParse : Parser<stm> =
        pid .>*> pstring ":=" .>*>. AexpParse |>> Ass <?> "Let"

    let DeclareParse : Parser<stm> =
        pdeclare >>. spaces1 >>. pid |>> Declare <?> "Declare"
        
    let SemiColonParse : Parser<stm> =
        StmntParseSecondary .>*> pchar ';' .>*>. StmntParsePrimary |>> Seq <?> "Seq"
    
    let IfThenElseParse : Parser<stm> =
        pif
         >*>. parenthesise BexpParse
        .>*> pthen
        .>*>. curlybracketise StmntParseSecondary
        .>*> pelse
        .>*>. curlybracketise StmntParseSecondary
        |>> (fun ((boolExp, trueStm), falseStm) -> ITE (boolExp, trueStm, falseStm)) <?> "IfElse"
    
    let IfThenParse : Parser<stm> =
        pif
         >*>. parenthesise BexpParse
        .>*> pthen
        .>*>. curlybracketise StmntParseSecondary
        |>> (fun (boolExp, trueStm) -> ITE (boolExp, trueStm, Skip)) <?> "IfThen"
        
    let WhileParse : Parser<stm> =
        pwhile
         >*>. parenthesise BexpParse    
        .>*> pdo
        .>*>. curlybracketise StmntParseSecondary
        |>> While <?> "While"

    do spref.Value <- choice
        [
            SemiColonParse
            StmntParseSecondary
        ]

    do ssref.Value <- choice
        [
            LetParse
            DeclareParse
            IfThenElseParse
            IfThenParse
            WhileParse
        ]

    let stmntParse = StmntParsePrimary

    // Inserted from newest project template
    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // From exercise 6.12
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    let stmntToSquareFun (stmnt: stm) : squareFun =
        fun (word: word) (pos: int) (acc: int) ->
            let state = mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] word ["_pos_"; "_acc_"; "_result_"]
            stmntEval2 stmnt >>>= lookup "_result_" |> evalSM state

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // From exercise 6.13
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    let returnIfFound (m: Map<int, 'a>) (i: int) : SM<'a option> =
        match m.TryFind i with
        | Some x -> ret <| Some x
        | _ -> ret None
    
    let stmntToBoardFun (stmnt: stm) (m: Map<int, 'a>) : coord -> Result<'a option, Error> =
       fun ((x, y): coord) ->
           let _a = 0
           let state = mkState [("_result_", 0); ("_x_", x); ("_y_", y)] [] ["_x_"; "_y_"; "_result_"]
           
           stmntEval2 stmnt >>>=
           lookup "_result_" >>=
           (returnIfFound m)
           |> evalSM state

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.12
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    
    let sourceToSquareFun source = 
        (run stmntParse >> getSuccess >> stmntToSquareFun) source
    
    let sourceToSquareFun' _ = sourceToSquareFun
    
    let parseSquareProg (sqp : squareProg) : square =
        
        // Non-parallel version:
        // Map.map sourceToSquareFun' sqp
        
        // Parallel version
        Map.toArray sqp
        |> Array.Parallel.map(fun (k, v) ->
            (k, sourceToSquareFun v))
        |> Map.ofArray

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.13
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    
    let parseBoardProg (source: string) (squares: Map<int, square>) : boardFun2 =
        run stmntParse source
        |> getSuccess
        |> fun stmnt -> stmntToBoardFun stmnt squares
    
    let parseBoardFun = parseBoardProg

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Exercise 7.14
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    let mkBoard (bp : boardProg) : board =
        // Non-parallel version
        //let m' = Map.map (fun _ sq -> parseSquareProg sq) bp.squares
        
        // Parallel version
        let m'' = Map.toArray bp.squares
                    |> Array.Parallel.map (fun (k, v) ->
                        (k, parseSquareProg v))
                    |> Map.ofArray
        
        {
            squares = parseBoardProg bp.prog m''
            center = bp.center
            defaultSquare = m''.[bp.usedSquare]
        }
