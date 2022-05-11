namespace NiwlSamdVibr

open MultiSet
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    
    type placedTile = coord * (uint32 * (char * int))
    type placedWord = placedTile list
    
    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        numPlayers    : uint32
        playerTurn    : uint32
        hand          : MultiSet.MultiSet<uint32>
        boardTiles    : Map<coord, char>
        lastPlayedTile : coord
        secondLastPlayedTile: coord
        timesPassed   : uint32
        tilesLeft : uint32

        
        // Hvilke brikker ligger der?? (Map<coord, en slags tile>)
    }

    let mkState b d pn np pt h bt cf cs tp tl= {board = b; dict = d;  playerNumber = pn; numPlayers = np; playerTurn = pt; hand = h; boardTiles = bt; lastPlayedTile=cf; secondLastPlayedTile=cs; timesPassed = tp; tilesLeft=tl}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let numPlayers st = st.numPlayers
    let playerTun st = st.playerTurn
    let hand st          = st.hand
    let boardTiles st   = st.boardTiles
    let lastPlayedTile st = st.lastPlayedTile
    let secondLastPlayedTile st = st.secondLastPlayedTile
    let timesPassed st = st.timesPassed
    let tilesLeft st = st.tilesLeft
    
    //let removeFromHand ms (st : state) : state =
        //st.hand
        
    //let rec findWord st =
      //  MultiSet.fold (fun acc key value ->
            // find char by key
            // step into dict with the char
            // remove char from hand
            // call findWord recursively
            
           // ) [] st.hand

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            //printfn "Updated Map: %A" st.boardTiles
            
            if st.playerTurn = st.playerNumber then
                
                //Er det vores tur? Spil, ellers ej - husk at opdater hvis tur det er
                //Hvis en siger forfeit game, så kan man ikke få turen
                
                let rec addAmount tileSet list (amount:uint) =
                    match amount with
                    | 0u -> list
                    | _ -> addAmount tileSet (tileSet::list) (amount-1u)
                
                let findTiles pieces hand = MultiSet.fold (fun acc id amount-> addAmount (Map.find id pieces) acc amount) [] hand
                let newHand  = findTiles pieces st.hand
                
                let charsInHand : list<list<char>> = List.fold(fun acc set -> (Set.fold(fun acc pair -> fst pair :: acc) [] set) :: acc) [] newHand
                
                let chars1  = List.fold(fun acc set -> (Set.fold(fun acc pair -> fst pair :: acc) [] set) @ acc) [] newHand
                
                let pointValues = List.fold(fun acc set -> (Set.fold(fun acc pair -> snd pair :: acc) [] set) @ acc) [] newHand
                
                //printfn "PIECES IN HAND %A" st.hand.Count
                printfn "CHARS IN HAND %A" (List.length charsInHand)   
                
                let rec add tileSet list (amount:uint) =
                    match amount with
                    | 0u -> list
                    | _ -> add tileSet (tileSet::list) (amount-1u)
                    
                let id = MultiSet.foldBack (fun id n acc -> add id acc n) st.hand []
                
                
                let removeElementFromHand (hand: char list list) (char: char list) =
                    List.fold(fun (b, newList) current -> if current=char && not b then (true, newList) else (b,current::newList)) (false, []) hand |> snd
            
                let rec findFirstWord (hand: char list list) (D : Dictionary.Dict )(currentWord : (bool * char) list) (FoundWord : (bool * char) list) : (bool * char) list=
                    let aux (i, acc) (e : char list) =
                      let boolFlag = List.length e > 1
                      List.fold(fun state element ->  
                          match Dictionary.step element D with
                          | Some (true, Drest) -> (i+1, findFirstWord (removeElementFromHand hand e) Drest ((boolFlag, element)::currentWord) ((boolFlag, element)::currentWord))
                          | Some (false, Drest) -> (i+1, findFirstWord (removeElementFromHand hand e) Drest ((boolFlag, element)::currentWord) (snd state))
                          | None -> state) (i,acc) e
                    match hand with
                    | [] -> FoundWord
                    | hand1 -> List.fold aux (0, FoundWord) hand1 |> snd
                    
                    //Lav hjælpemetode der tjekker om current FoundWord er større end foundWord. Kald recursivt med det længste.
                let helpFindLongestWord (FoundWord : (bool * char) list) (currentWord : (bool * char) list)=
                    if List.length FoundWord > List.length currentWord then FoundWord else currentWord
                
                
                let rec findWords (directionCoord : coord) (hand: char list list) (D : Dictionary.Dict ) (currentWord : (bool * char) list) (FoundWord : (bool * char) list) : coord * (bool * char) list =
                    let aux (acc) (e : char list) =
                      let boolFlag = List.length e > 1
                      
                      List.fold(fun state element ->  
                          match Dictionary.step element D with
                          | Some (true, Drest) -> (findWords directionCoord (removeElementFromHand hand e) Drest ((boolFlag, element)::currentWord)  (helpFindLongestWord FoundWord ((boolFlag, element)::currentWord)))
                          | Some (false, Drest) -> (findWords directionCoord (removeElementFromHand hand e) Drest ((boolFlag, element)::currentWord) (snd state))
                          | None -> state) (acc) e
                    
                    //Hvis der ligger noget stepper vi nedad i dictionary og kalder findwor dmed næste koordinat. Og hvis dictionary giver noget tilbage. 
                   
                    match hand with
                    | [] -> (directionCoord, FoundWord)
                    | hand1 -> List.fold aux (directionCoord, FoundWord) hand1  
                     
                //start of the game. If the map is empty findword from hand and play it.
                let charToIntMapAlphabet = Map.add 'A' 1u Map.empty |> Map.add 'B' 2u |> Map.add 'C' 3u |> Map.add 'D' 4u |> Map.add 'E' 5u |> Map.add 'F' 6u
                                           |> Map.add 'G' 7u|> Map.add 'H' 8u |> Map.add 'I' 9u |> Map.add 'J' 10u |> Map.add 'K' 11u |> Map.add 'L' 12u
                                           |> Map.add 'M' 13u |> Map.add 'N' 14u |> Map.add 'O' 15u |> Map.add 'P' 16u |> Map.add 'Q' 17u |> Map.add 'R' 18u
                                           |> Map.add 'S' 19u |> Map.add 'T' 20u |> Map.add 'U' 21u |> Map.add 'V' 22u |> Map.add 'W' 23u |> Map.add 'X' 24u
                                           |> Map.add 'Y' 25u |> Map.add 'Z' 26u
                
                
                //Ideen er at have en startCoord. Hvor der ligger et bogstac. ListofSquares er dem der ligger rundt om startcoord.                            
                (*let rec checkAroundTile (startCoord : coord) (newCoord : coord) (index : int) =
                    let yDown = ((fst newCoord), (snd newCoord + 1))
                    let yUp = ((fst newCoord), (snd newCoord - 1))
                    let xLeft = ((fst newCoord - 1), (snd newCoord))
                    let xRight = ((fst newCoord + 1), (snd newCoord))
                    let listOfSquaresAround = [yDown; yUp; xLeft; xRight]
                    match Map.tryFind newCoord st.boardTiles with
                    | Some v -> checkAroundTile startCoord listOfSquaresAround.[index] (index+1)
                    | None -> match Dictionary.step (Map.find startCoord st.boardTiles) st.dict with
                                |Some (v, dic) -> findWords newCoord charsInHand dic [(false, Map.find startCoord st.boardTiles)] []
                                | None -> ((-1,-1), [])*)
                

                let findWordFromGivenTile (startCoord:coord)=                
                    match Map.tryFind startCoord st.boardTiles with
                    | Some v -> match Dictionary.step (Map.find startCoord st.boardTiles) st.dict with
                                |Some (v, dic) -> findWords startCoord charsInHand dic [(false, Map.find startCoord st.boardTiles)] []
                                | None -> ((0,0),findFirstWord charsInHand st.dict [] [])
                    |None -> ((0,0),findFirstWord charsInHand st.dict [] [])

                
                //val ms: (coord * (uint32 * (char * int))) list                                                     
                let rec constructMove (charsInHand: (bool * char) list) (move: list<((int * int) * (uint32 * (char * int)))>) (index : (int*int)) =
                   let aux nyListe stadie =
                       let isBlankTile = List.item ((fst index)-1) charsInHand |> fst
                       let charAndPointValue = Set.minElement (Map.find (Map.find (List.item ((fst index)-1) charsInHand |> snd) charToIntMapAlphabet) pieces)   
                       let tileNormal = (((snd index),0):coord),((Map.find (List.item ((fst index)-1) charsInHand |> snd) charToIntMapAlphabet),
                                                           Set.minElement (Map.find (Map.find (List.item ((fst index)-1) charsInHand |> snd) charToIntMapAlphabet) pieces))  
                       let tileJoker = ((snd index,0):coord), (0u, (fst charAndPointValue, 0)) //Jokertile giver altid 0 point
                       let tileFinal = if not isBlankTile then tileNormal else tileJoker
                       match stadie with
                       | (i,n) -> constructMove charsInHand (tileFinal::nyListe) (i-1,n-1)
                   match index with
                   | (0,_) -> move
                   | (i,n) -> aux move (i,n)
                

                
                let rec constructMoveDownWards (charsInHand: coord * (bool * char) list) (move: list<((int * int) * (uint32 * (char * int)))>) (startingCoord: coord) (index: int) (direction: string)=
                    let isBlankTile = List.item index (snd charsInHand) |> fst
                    let charPointValue = Set.minElement (Map.find (Map.find (List.item index (snd charsInHand) |> snd) charToIntMapAlphabet) pieces)
                    let uintValue = (Map.find (List.item index (snd charsInHand) |> snd) charToIntMapAlphabet)
                    let tileNormal = (startingCoord,(uintValue,(charPointValue)))
                    
                    let tileJoker = (startingCoord), (0u, (fst charPointValue, 0)) //Jokertile giver altid 0 point
                    let tileFinal = if not isBlankTile then tileNormal else tileJoker
                    
                    match index with
                    | v -> if v = 0 then move@[tileFinal] else constructMoveDownWards charsInHand (move@[tileFinal]) (fst startingCoord, snd startingCoord+1) (index-1) direction
                
                let rec constructMoveRight (charsInHand: coord * (bool * char) list) (move: list<((int * int) * (uint32 * (char * int)))>) (startingCoord: coord) (index: int) (direction: string)=
                    let isBlankTile = List.item index (snd charsInHand) |> fst
                    let charPointValue = Set.minElement (Map.find (Map.find (List.item index (snd charsInHand) |> snd) charToIntMapAlphabet) pieces)
                    let uintValue = (Map.find (List.item index (snd charsInHand) |> snd) charToIntMapAlphabet)
                    let tileNormal = (startingCoord,(uintValue,(charPointValue)))
                    
                    let tileJoker = (startingCoord), (0u, (fst charPointValue, 0)) //Jokertile giver altid 0 point
                    let tileFinal = if not isBlankTile then tileNormal else tileJoker
                    
                    match index with
                    | v -> if v = 0 then move@[tileFinal] else constructMoveRight charsInHand (move@[tileFinal]) (fst startingCoord+1, snd startingCoord) (index-1) direction
                

                let constructMoveHelperFunc (charsInHand: coord * (bool * char) list) (move: list<((int * int) * (uint32 * (char * int)))>) (startingCoord: coord) (index: int) (direction: string)=
                    match direction with
                    | "down" -> constructMoveDownWards charsInHand move (fst startingCoord, snd startingCoord+1) index direction
                    | "right" -> if startingCoord = (0,0) then constructMoveDownWards charsInHand move startingCoord index direction else constructMoveRight charsInHand move (fst startingCoord+1, snd startingCoord) index direction
                    //| "rightFirstMove"-> constructMoveRight charsInHand [] (0,0) 
          
                let checkDownWardsTile (startCoord: coord)  =
                    let koordinatTop = ((fst startCoord), (snd startCoord-1))
                    let koordinatLeft = ((fst startCoord-1), (snd startCoord))
                    let koordinatRight = ((fst startCoord+1),(snd startCoord))                
                    let koordinatDownwards = ((fst startCoord),(fst startCoord+1))
                    let aux  =
                        match Map.tryFind koordinatTop st.boardTiles with
                        | Some v ->  "right"
                        | None -> " " //Burde aldrig ske
                    match Map.tryFind koordinatLeft st.boardTiles with
                    | Some v -> "down"
                    | None -> aux 
                
                let rec constructNextMove (charsInHand: coord * (bool * char) list) =
                   let directionReal = if fst charsInHand = (0,0) then "right" else checkDownWardsTile (fst charsInHand)
                   let UpdatedHand = if fst charsInHand = (0,0) then charsInHand else (fst charsInHand, (snd charsInHand).[0..(List.length (snd charsInHand)-2)])  
                   let constructMove = constructMoveHelperFunc UpdatedHand [] (fst charsInHand) (List.length (snd UpdatedHand)-1 ) directionReal
                   constructMove
                   
                let selectLastInsertedKey = st.lastPlayedTile

                let playRestOfMoves1 = findWordFromGivenTile st.lastPlayedTile  //spil på Sidst Placerede tile
                
                //Check om der ligger noget på siderne af tiles i dette move. Hvis der går kan moved ikke spilles. Index 0 er det bogstav der ligger på boardet. Tjek på siderne af bogstavet i index 1.
                let playRestOfMoves2 = findWordFromGivenTile st.secondLastPlayedTile //spil på næstsidst placerede tile
                

                //check om der er fundet et ord ellers prøv med andet sidste placerede
                let checkOrFindWithSecondLasts =  if List.isEmpty (snd playRestOfMoves1) then playRestOfMoves2 else playRestOfMoves1
                
                //Hvis både playRestOfMoves1 og playRestOfMoves2 er tomme skal man enten "passe" eller få en ny hånd (discarde alle sine brikker)
                
                //let checkIfYouShouldPassOrGetNewHand = if playRestOfMoves1 && playRestOfMoves2 then //pass else check
                
                let move = if List.isEmpty (snd checkOrFindWithSecondLasts) then [((0,0), (0u ,('a',-100)))] else (constructNextMove (checkOrFindWithSecondLasts))       
                
                let a = "HEJ"
                           
                // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                
                (*let input =  System.Console.ReadLine()
                let move = RegEx.parseMove input*)

                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
               
                if st.tilesLeft = 0u then send cstream (SMPass)

                printfn "::::::: ID :::: %A" id
                if move = [((0,0), (0u ,('a',-100)))] then send cstream (SMChange id) else send cstream (SMPlay move)
                
                //then (if st.timesPassed > 1u then send cstream (CMChangeSuccess) else send cstream (SMPass))
                debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            let msg = recv cstream

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                //Updating hand
                let lst = List.map (fun (_, (u, _)) -> u) ms
                let deletedSet = List.fold(fun acc x -> MultiSet.removeSingle x acc) st.hand lst
                
                let rec appendMultiple k v lst=
                    match v with
                    | 0u -> lst
                    | _ -> appendMultiple k (v-1u) (k :: lst)
                
                let lst1 : list<uint32> = List.fold(fun acc ((k : uint32), (v : uint32)) -> if v > 1u then (appendMultiple k v acc) else k :: acc ) [] newPieces

                //let lst1 = List.map (fun (u, _) -> u) newPieces
                
                let newSet = List.fold(fun acc x -> MultiSet.addSingle x acc) deletedSet lst1
                
                //Updating board
                //val ms: (coord * (uint32 * (char * int))) list
                let updateBoard = List.fold(fun x (coord,(_,(c,_)))-> Map.add coord c x) st.boardTiles ms
                
                let lastPlayedOnThisTile =fst (List.item ((List.length ms)-1) ms)
                let secondLastPlayedTile =fst (List.item ((List.length ms)-2) ms)
                
                let playerTurn = st.playerTurn % st.numPlayers + 1u
                
                let timesPassed = 0u

                let lengtOfMove = List.length ms

                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = {st with hand = newSet; boardTiles = updateBoard; lastPlayedTile=lastPlayedOnThisTile; secondLastPlayedTile=secondLastPlayedTile; playerTurn = playerTurn; timesPassed = timesPassed}// This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                
                let lastPlayedOnThisTile =fst (List.item ((List.length ms)-1) ms)
                let secondLastPlayedTile =fst (List.item ((List.length ms)-2) ms)
                
                let updateBoard = List.fold(fun x (coord,(_,(c,_)))-> Map.add coord c x) st.boardTiles ms
                let playerTurn = st.playerTurn % st.numPlayers + 1u
                let timesPassed = 0u
             
                let st' = {st with boardTiles = updateBoard; lastPlayedTile=lastPlayedOnThisTile; secondLastPlayedTile=secondLastPlayedTile; playerTurn = playerTurn; timesPassed = timesPassed}// This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                //let playerTurn = st.playerTurn % st.numPlayers + 1u
                //let st' = {st with playerTurn = playerTurn} // This state needs to be updated
                let st' = st
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM (CMPassed (playerId)) ->  //Ændrer turen                
                let playerTurn = st.playerTurn % st.numPlayers + 1u
                let timesPassed = st.timesPassed + 1u
                let st' = {st with playerTurn = playerTurn; timesPassed = timesPassed}
                aux st'
            | RCM (CMChangeSuccess(newTiles)) ->
                printfn ":::::---------------::::::::"
                printfn "CHANING TILES %A" newTiles
                (*let rec appendMultiple k v lst=
                    match v with
                    | 0u -> lst
                    | _ -> appendMultiple k (v-1u) (k :: lst)
                let lst : list<uint32> = List.fold(fun acc ((k : uint32), (v : uint32)) -> if v > 1u then (appendMultiple k v acc) else k :: acc ) [] newTiles
                let newSet = List.fold(fun acc x -> MultiSet.addSingle x acc) MultiSet.empty lst*)
                let newSet = List.fold(fun acc (id, amount) -> MultiSet.add id amount acc) MultiSet.empty newTiles
                printfn "NEW TILLLESSSS :^^^^ %A" newTiles
                let timesPassed = 0u
                let playerTurn = st.playerTurn % st.numPlayers + 1u
                let st' = {st with hand = newSet; timesPassed = timesPassed; playerTurn = playerTurn}
                printfn "NEW HAAAAND :^^^^ %A" newSet
                aux st'
            | RCM (CMChange (playerId, numberOfTiles)) ->
                printfn "Player changed tiles: %A, %A" playerId numberOfTiles
                let playerTurn = st.playerTurn % st.numPlayers + 1u
                let st' = {st with playerTurn = playerTurn}
                aux st'
            //| RCM (CMForfeit (playerId)) -> // Turen må ikke gå til den "forfeitede" spiller igen
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                let tilesLeft =
                    List.fold (fun acc element ->
                        match element with
                        | GPENotEnoughPieces(_, piecesLeft) -> piecesLeft 
                        | _ -> acc
                        ) st.tilesLeft err
                printfn "Gameplay Error:\n%A" err; aux st
                let playerTurn = st.playerTurn % st.numPlayers + 1u
                let st' = {st with playerTurn = playerTurn; tilesLeft = tilesLeft}
                aux st'             
        aux st
        
        
        
        (*let result =
            List.fold
                (fun state element ->
                    match element with
                    | GPENotEnoughPieces(a,b) ->
                        let s = {st with tilesLeft = b}
                        s
                    | _ -> printfn "Gameplay Error:\n%A" element; state
                    ) st err
    aux result*)

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32)  // hold styr på
            (playerNumber : uint32)  // hold styr på
            (playerTurn  : uint32) // Hold styr på
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>) // !!!
            (timeout : uint32 option)
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
        
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber numPlayers playerTurn handSet Map.empty (0,0) (0,0) 0u (MultiSet.size handSet))
        