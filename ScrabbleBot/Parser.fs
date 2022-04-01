// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
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
    let pIsVowel   = pstring "isVowel"
    
    let pif  = pstring "if"       
        
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy (System.Char.IsWhiteSpace)<?> "whitespace"
    let pletter        = satisfy (System.Char.IsLetter) <?> "letter"
    let palphanumeric  = satisfy(System.Char.IsLetterOrDigit) <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many1 whitespaceChar<?> "space1"

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2 
    let (.>*>) p1 p2 = (p1 .>> spaces) .>> p2 
    let (>*>.) p1 p2 = (p1 .>> spaces) >>. p2 

    let parenthesise p = (pchar '(' >*>.  p) .>*> pchar ')'

    
    let pid  = pletter <|> pchar '_' .>>. many (palphanumeric <|> pchar '_') |>> fun(char, charList) -> List.fold(fun acc current -> acc+ (string current)) (string char) charList 

    let unop anychar p1 = anychar  >*>. p1 
    let binop p p1 p2 = ((p1 .>*> p) .>*>. p2)

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    
    let cXParse, cref = createParserForwardedToRef<cExp>()
    
    let bExpParseOne, brefCon = createParserForwardedToRef<bExp>()
    let bXParseTwo, brefEqual = createParserForwardedToRef<bExp>()
    let bXParseThree, brefisLetter = createParserForwardedToRef<bExp>()
    
    let seqParse, seqref = createParserForwardedToRef<stm>()
    let stmParse, sref = createParserForwardedToRef<stm>()




    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"

    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [SubParse;AddParse; ProdParse]
    
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    
    let charToInt = pCharToInt >*>. parenthesise cXParse  |>> CharToInt <?> "charToInt"
    do pref := choice [MulParse; DivParse; ModParse;charToInt ;AtomParse]
        
    let PointValueParse = unop (pstring "pointValue") AtomParse |>> PV <?> "PV"
   
    let NParse   = pint32 |>> N <?> "Int"
    let VParse = pid |>> V <?> "Variable"
    
    let PNeg = unop (pchar '-') AtomParse |>> (fun x -> Mul(N -1 ,x))  <?> "Neg"
        
        
    let ParParse = parenthesise TermParse
    do aref := choice [PNeg;PointValueParse;  NParse ; VParse; ParParse; ]

    let AexpParse = TermParse 

//7.9
    
    let CParse = pchar ''' >>. anyChar .>> pchar '''  |>> C <?> "C"
    let toUpper = unop (pstring "toUpper") (parenthesise cXParse) |>> ToUpper  <?> "toUpper"
    let toLower = unop (pstring "toLower")  (parenthesise cXParse) |>> ToLower  <?> "toLower"
    
    let intToChar = unop (pstring "intToChar")  (parenthesise TermParse) |>> IntToChar <?> "intToChar"
    let charValue = unop (pstring "charValue") TermParse |>> CV <?> "charValue"


    do cref := choice[CParse; toUpper; toLower; intToChar; charValue]
    
    
    (*let charValue
    let intToChar
    let toUpper
    let toLower*)
    
    let CexpParse = cXParse
    
//7.10
    let BexpParse = bExpParseOne
    
    let bTrue =  pstring "true"  |>> fun _ -> TT
    let bFalse = pstring "false" |>> fun _ -> FF
    
    let bConParse = binop (pstring "/\\") bXParseTwo bExpParseOne |>> Conj <?> "B/\B"
    let bDisParse = binop (pstring "\/") bXParseTwo bExpParseOne |>> fun(x,y) -> (x .||. y)
    //Venstre side skal være næst laveste hieraki (bXParseTwo), for ellers vil man få left recursion og derfor stackOverflow.
    //Basicly: Når vi har fundet en conjunction skal den ikke lede efter flere conjuction på venstre side.
    //Dog kan der være masser af conjunctions på højre side, men der kommer ikke stack overflow. 
    
    do brefCon := choice[ bConParse;bDisParse; bXParseTwo]
        
    let equal =  binop (pstring "=") AtomParse AtomParse |>> fun(x,y) -> AEq(x,y)
    
    let equalOrNot = binop (pstring "<>") AtomParse AtomParse |>> fun (x,y) ->x .<>.y
    let biggerThan = binop (pstring "<") AtomParse AtomParse |>> fun (x,y) ->x .<.y
    let lessThan = binop (pstring ">") AtomParse AtomParse |>> fun (x,y) ->x .>.y
    let equalOrbigger = binop(pstring ">=") AtomParse AtomParse |>> fun(x,y) -> x.>=.y
    let equalOrLess = binop(pstring "<=") AtomParse AtomParse |>> fun(x,y) -> x.<=.y
        
    do brefEqual := choice[equal;equalOrNot;lessThan;equalOrLess;biggerThan;equalOrbigger; bXParseThree]
        
    let negation = unop (pstring "~") bXParseThree |>> Not <?> "Not"
    let removeParenthsis = parenthesise bExpParseOne 
    
    //let isLet = unop 
    
    do brefisLetter := choice[negation;bTrue;bFalse; removeParenthsis]
    //7.11
    let stmntParse = seqParse
    
    // Sterm [Seq; Satom]
    // Satom [ alt andet ]
    
    let value = binop (pstring ":=") pid AexpParse |>> Ass <?> "ass"
    pid .>*> pstring ":=" .>*>. AexpParse
    
    let declareStm = pdeclare >>. spaces1 >>. pid |>> Declare <?> "declare"
    
    let parseS = stmParse .>*> pchar ';' .>*>. seqParse |>> Seq <?> "seq"
    
    do seqref := choice [parseS; stmParse]
    do sref:= choice[value;declareStm]

(* These five types will move out of this file once you start working on the project *)
    (*type coord      = int * int
    type squareProg = Map<int, string>
    type boardProg  = {
            prog       : string;
            squares    : Map<int, squareProg>
            usedSquare : int
            center     : coord
    
            isInfinite : bool   // For pretty-printing purposes only
            ppSquare   : string // For pretty-printing purposes only
        }*)

    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>

    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    type boardFun2 = coord -> Result<square option, Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}

