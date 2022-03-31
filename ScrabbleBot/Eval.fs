// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let binop f a b  =
        a >>= fun x ->
        b >>= fun y ->
            ret (f x y)
 
    let nonzeroBinop f a b =
        a >>= fun x ->
        b >>= fun y ->
            if y <> 0 then ret (f x y) else fail DivisionByZero
            
    let div (a: SM<int>) (b: SM<int>) : SM<int> =
         nonzeroBinop (/) a b
             
    let mod' (a: SM<int>) (b: SM<int>) : SM<int> =
        nonzeroBinop (%) a b

    let rec arithEval (a: aExp) : SM<int> =
        match a with
        | N n -> ret n
        | V str -> lookup str
        | WL -> wordLength
        | PV a' -> arithEval a' >>= pointValue
        | Add (a, b) -> binop ( + ) (arithEval a) (arithEval b)
        | Sub (a, b) -> binop ( - ) (arithEval a) (arithEval b)
        | Mul (a, b) -> binop ( * ) (arithEval a) (arithEval b)
        | Div (a, b) -> div (arithEval a) (arithEval b)
        | Mod (a, b) -> mod' (arithEval a) (arithEval b)
        | CharToInt c -> charEval c >>= (int >> ret)

    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV aExp -> arithEval aExp >>= characterValue
        | ToUpper cExp -> charEval cExp >>= (System.Char.ToUpper >> ret)
        | ToLower cExp -> charEval cExp >>= (System.Char.ToLower >> ret)
        | IntToChar aExp -> arithEval aExp >>= (char >> ret)   


    let isConsonant': char -> bool
        = fun c ->
            match System.Char.ToLower(c) with
            | 'a'|'e'|'i'|'o'|'u'|'y' -> false
            | _ -> true
        
    let rec boolEval b : SM<bool> =
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a, b) -> binop (=) (arithEval a) (arithEval b)
        | ALt (a, b) -> binop (<) (arithEval a) (arithEval b)
        | Not a -> boolEval a >>= (ret << not)
        | Conj (a, b) -> binop (&&) (boolEval a) (boolEval b)
        | IsVowel cExp -> charEval cExp >>= (isConsonant' >> not >> ret)
        | IsLetter cExp -> charEval cExp >>= (System.Char.IsLetter >> ret)
        | IsDigit cExp -> charEval cExp >>= (System.Char.IsDigit >> ret)


    type stm =                    (* statements *)
        | Declare of string       (* variable declaration *)
        | Ass of string * aExp    (* variable assignment *)
        | Skip                    (* nop *)
        | Seq of stm * stm        (* sequential composition *)
        | ITE of bExp * stm * stm (* if-then-else statement *)
        | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare str -> declare str
        | Ass (key, value) -> arithEval value >>= (update key)
        | Skip -> ret ()
        | Seq (step1, step2) -> stmntEval step1 >>>= stmntEval step2
        | ITE (cond, condTrueStmnt, condFalseStmnt) ->
            boolEval cond >>= fun cond' ->
                match cond' with
                | true -> push >>>= stmntEval condTrueStmnt >>>= pop
                | false -> push >>>= stmntEval condFalseStmnt >>>= pop
        | While (cond, doStmnt) ->
            boolEval cond >>= fun cond' ->
                match cond' with
                | true -> push >>>= stmntEval doStmnt >>>= pop >>>= stmntEval stmnt
                | false -> ret ()

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a =
        prog {
            match a with
            | N n -> return n
            | V str ->
                let! res = lookup str
                return res
            | WL ->
                let! res = wordLength
                return res
            | PV a' -> 
                let! pos = arithEval a'
                let! pv = pointValue pos
                return pv
            | Add (a, b) -> return! binop ( + ) (arithEval a) (arithEval b)
            | Sub (a, b) -> return! binop ( - ) (arithEval a) (arithEval b)
            | Mul (a, b) -> return! binop ( * ) (arithEval a) (arithEval b)
            | Div (a, b) -> return! div (arithEval a) (arithEval b)
            | Mod (a, b) -> return! mod' (arithEval a) (arithEval b)
            | CharToInt c ->
                let! c = charEval c
                return (int c)
        }

        
    let charEval2 c =
        prog {
            match c with
            | C c -> return c
            | CV aExp ->
                let! pos = arithEval aExp
                return! characterValue pos
            | ToUpper cExp ->
                let! oldChar = charEval cExp
                return System.Char.ToUpper oldChar
            | ToLower cExp ->
                let! oldChar = charEval cExp
                return System.Char.ToLower oldChar
            | IntToChar aExp ->
                let! n = arithEval aExp
                return char n
         }
    
    
    let rec boolEval2 b =
        prog {
            match b with
            | TT -> return true
            | FF -> return false
            | AEq (a, b) -> return! binop (=) (arithEval a) (arithEval b)
            | ALt (a, b) -> return! binop (<) (arithEval a) (arithEval b)
            | Not a ->
                let! res = boolEval2 a
                return not res
            | Conj (a, b) -> return! binop (&&) (boolEval a) (boolEval b)
            | IsVowel cExp ->
                let! c = charEval2 cExp
                return (not << isConsonant') c
            | IsLetter cExp ->
                let! c = charEval2 cExp
                return System.Char.IsLetter c
            | IsDigit cExp ->
                let! c = charEval2 cExp
                return System.Char.IsDigit c
        }
         
    let rec stmntEval2 stmnt =
        prog {
            match stmnt with
            | Declare str -> return! declare str
            | Ass (key, value) ->
                let! value = arithEval2 value
                return! update key value
            | Skip -> return ()
            | Seq (step1, step2) ->
                let! res = prog.Combine(stmntEval2 step1, stmntEval2 step2)
                return res
            | ITE (cond, condTrueStmnt, condFalseStmnt) ->
                let! cond' = boolEval2 cond
                if cond' then
                    do! push
                    do! stmntEval2 condTrueStmnt
                    do! pop
                    return ()
                else
                    do! push
                    do! stmntEval2 condFalseStmnt
                    do! pop
                    return ()
            | While (cond, doStmnt) ->
                let! cond' = boolEval cond
                if cond' then
                    do! push
                    do! stmntEval2 doStmnt
                    do! pop
                    do! stmntEval2 stmnt
                    return ()
                else
                    return ()
        }

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun (stmnt: stm) : squareFun =
        fun (word: word) (pos: int) (acc: int) ->
            let state = mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] word ["_pos_"; "_acc_"; "_result_"]
            stmntEval stmnt >>>= lookup "_result_" |> evalSM state

    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun (stmnt: stm) (m: Map<int, squareFun>) : boardFun =
        fun ((x, y): coord) ->
            let state = mkState [("_result_", 0); ("_x_", x); ("_y_", y)] [] ["_x_"; "_y_"; "_result_"]
            match stmntEval2 stmnt >>>= lookup "_result_" |> evalSM state with
            | Success i ->
                match Map.tryFind i m with
                | None    -> Success None
                | someSF  -> Success someSF
            | Failure f -> Failure f

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let transformIds (ids: (int * stm) list) : Map<int, squareFun> =
        ids |> List.map (fun (i, stmnt) -> (i, stmntToSquareFun stmnt)) |> Map.ofList

    let mkBoard (c: coord) (defaultSq: stm) (boardStmnt: stm) (ids: (int * stm) list) : board =
        { center = c;
            defaultSquare = stmntToSquareFun defaultSq;
            squares = transformIds ids |> stmntToBoardFun boardStmnt }