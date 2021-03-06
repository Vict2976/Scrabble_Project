namespace NiwlSamdVibr

open MultiSet
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open StateMonad

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
        timesPassed   : uint32
        tilesLeft : uint32

    }

    let mkState b d pn np pt h bt tp tl= {board = b; dict = d;  playerNumber = pn; numPlayers = np; playerTurn = pt; hand = h; boardTiles = bt; timesPassed = tp; tilesLeft=tl}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let numPlayers st = st.numPlayers
    let playerTun st = st.playerTurn
    let hand st          = st.hand
    let boardTiles st   = st.boardTiles
    let timesPassed st = st.timesPassed
    let tilesLeft st = st.tilesLeft
    
    
module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            
            let rec addAmount tileSet list (amount:uint) =
                match amount with
                | 0u -> list
                | _ -> addAmount tileSet (tileSet::list) (amount-1u)
            
            let findTiles pieces hand = MultiSet.fold (fun acc id amount-> addAmount (Map.find id pieces) acc amount) [] hand
            let newHand  = findTiles pieces st.hand
            
            let charsInHand : list<list<char>> = List.fold(fun acc set -> (Set.fold(fun acc pair -> fst pair :: acc) [] set) :: acc) [] newHand
            
 
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
                               
                match hand with
                | [] -> (directionCoord, FoundWord)
                | hand1 -> List.fold aux (directionCoord, FoundWord) hand1  
                 
            //start of the game. If the map is empty findword from hand and play it.
            let charToIntMapAlphabet = Map.add 'A' 1u Map.empty |> Map.add 'B' 2u |> Map.add 'C' 3u |> Map.add 'D' 4u |> Map.add 'E' 5u |> Map.add 'F' 6u
                                       |> Map.add 'G' 7u|> Map.add 'H' 8u |> Map.add 'I' 9u |> Map.add 'J' 10u |> Map.add 'K' 11u |> Map.add 'L' 12u
                                       |> Map.add 'M' 13u |> Map.add 'N' 14u |> Map.add 'O' 15u |> Map.add 'P' 16u |> Map.add 'Q' 17u |> Map.add 'R' 18u
                                       |> Map.add 'S' 19u |> Map.add 'T' 20u |> Map.add 'U' 21u |> Map.add 'V' 22u |> Map.add 'W' 23u |> Map.add 'X' 24u
                                       |> Map.add 'Y' 25u |> Map.add 'Z' 26u

            let findWordFromGivenTile (startCoord:coord)=                
                match Map.tryFind startCoord st.boardTiles with
                | Some v -> match Dictionary.step (Map.find startCoord st.boardTiles) st.dict with
                            |Some (v, dic) -> findWords startCoord charsInHand dic [(false, Map.find startCoord st.boardTiles)] []
                            | None -> ((0,0),findFirstWord charsInHand st.dict [] [])
                |None -> ((0,0),findFirstWord charsInHand st.dict [] [])


            
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
            

            
            let getDirection (startCoord: coord)  =
                let koordinatTop = ((fst startCoord), (snd startCoord-1))
                let koordinatLeft = ((fst startCoord-1), (snd startCoord))
                let koordinatRight = ((fst startCoord+1),(snd startCoord))                
                let koordinatDownwards = ((fst startCoord),(fst startCoord+1))
                let checkDownwards =
                    match Map.tryFind koordinatDownwards st.boardTiles with
                    |Some v -> "right"
                    | None -> " " //burde aldrig ske nu 
                let aux  =
                    match Map.tryFind koordinatTop st.boardTiles with
                    | Some v ->  "right"
                    | None -> checkDownwards
                match Map.tryFind koordinatLeft st.boardTiles with
                | Some v -> "down"
                | None -> aux 
            
            let constructMoveHelperFunc (charsInHand: coord * (bool * char) list) (move: list<((int * int) * (uint32 * (char * int)))>) (startingCoord: coord) (index: int) (direction: string)=
                match direction with
                | "down" -> constructMoveDownWards charsInHand move (fst startingCoord, snd startingCoord+1) index direction
                | "right" -> if startingCoord = (0,0) then constructMoveDownWards charsInHand move startingCoord index direction else constructMoveRight charsInHand move (fst startingCoord+1, snd startingCoord) index direction
      
            let rec constructNextMove (charsInHand: coord * (bool * char) list) (direction: string)=
               let UpdatedHand = if fst charsInHand = (0,0) then charsInHand else (fst charsInHand, (snd charsInHand).[0..(List.length (snd charsInHand)-2)])  
               let constructMove = constructMoveHelperFunc UpdatedHand [] (fst charsInHand) (List.length (snd UpdatedHand)-1 ) direction
               constructMove

            let playALLMoves =
                Map.fold (fun state key value ->
                    let word = findWordFromGivenTile key
                    if snd word = [] then state else word::state) [] st.boardTiles
                    
            let setDirectionForAllMoves =
                List.fold(fun acc element -> ((getDirection (fst element)),(element)) ::acc) [] playALLMoves

            let rec makeCoordinatsRight (startcoord) (lengthOfList:int) (listOfCoords: list<int*int>) =
                //Når der skal spilles "right"
                match lengthOfList with
                | v -> if v > 0 then
                    let middleTile = (fst startcoord+1,snd startcoord)
                    let tileUp = (fst startcoord+1,snd startcoord-1)
                    let tileDown = (fst startcoord+1,snd startcoord+1)
                    let tileRightMiddle = (fst startcoord+2,snd startcoord)
                    makeCoordinatsRight middleTile (lengthOfList-1) ([middleTile;tileUp;tileDown;tileRightMiddle]@listOfCoords)
                        else listOfCoords
            
            let rec makeCoordinatsDown (startcoord) (lengthOfList:int) (listOfCoords: list<int*int>) =
                //Når der skal spilles "down"
                match lengthOfList with
                | v -> if v > 0 then

                    let middleTile = (fst startcoord,snd startcoord+1)
                    let tileleft = (fst startcoord-1,snd startcoord+1)
                    let tileRight = (fst startcoord+1,snd startcoord+1)
                    let tileDownMiddle = (fst startcoord,snd startcoord+2)
                    makeCoordinatsDown middleTile (lengthOfList-1) ([middleTile;tileleft;tileRight;tileDownMiddle]@listOfCoords)
                        else listOfCoords 
             
            let checkIfMoveIsPlayableOnBoard (direction:string ) (move :(coord * (bool * char) list)) =
                let lenghtOfWord = (List.length (snd move)-1) 

                let (a,b) = fst move
                if direction = "right" then
 
                    let listOfTilesToCheck = makeCoordinatsRight (fst move) lenghtOfWord []

                    let aux (koord:coord) lst =
                        match Map.tryFind koord st.boardTiles with
                        | Some v -> v::lst
                        | None -> lst
                    let returnedList = List.fold (fun acc element -> aux element acc) [] listOfTilesToCheck
                    List.isEmpty returnedList //Hvis listen er empty kan moved spilles, da der ikke liger noget omkring nogle af brikkerne.
                else
                    let listOfTilesCheck = makeCoordinatsDown (fst move) lenghtOfWord []
                    let aux (koord:coord) lst =
                        match Map.tryFind koord st.boardTiles with
                        | Some v -> v::lst
                        | None -> lst
                    let returnedList = List.fold (fun acc element -> aux element acc) [] listOfTilesCheck
                    List.isEmpty returnedList //Hvis listen er empty kan moved spilles, da der ikke liger noget omkring nogle af brikkerne.
            
            
            let AllMovesThatCanBePlayed = List.fold (fun acc element -> if (checkIfMoveIsPlayableOnBoard (fst element) (snd element)) = true then (element)::acc else acc ) [] setDirectionForAllMoves

            let playMove =
                if Map.isEmpty st.boardTiles then
                    let firstWord = findWordFromGivenTile (0,0) //Find første ord
                    let constructedMove = if List.isEmpty (snd firstWord) then [((0,0), (0u ,('a',-100)))] else constructNextMove firstWord "right" //
                    constructedMove
                else
                    let moveToPlay = if (List.isEmpty AllMovesThatCanBePlayed) then (" ",(((-100,-100 :coord),[true,'A']))) else  AllMovesThatCanBePlayed[0]
                    let moveBefore = if moveToPlay= (" ",(((-100,-100 :coord),[true,'A']))) then [((-100,-100), (0u ,('a',-100)))] else  constructNextMove (snd moveToPlay) (fst moveToPlay)
                    let move = if (fst moveBefore[0]) = (0,0) then [((-100,-100), (0u ,('a',-100)))] else moveBefore
                    move

           
                            
            let move = playMove

            let tileToHand = if st.tilesLeft < 7u then st.tilesLeft else 7u
            
            if move = [((-100,-100), (0u ,('a',-100)))] then send cstream (SMChange id[0.. (int tileToHand)-1]) else send cstream (SMPlay move)
                           
            // remove the force print when you move on from manual input (or when you have learnt the format)
            //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
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
                let newSet = List.fold(fun acc x -> MultiSet.addSingle x acc) deletedSet lst1
                let updateBoard = List.fold(fun x (coord,(_,(c,_)))-> Map.add coord c x) st.boardTiles ms
                let tilesLeft = st.tilesLeft - uint32(List.length ms)
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = {st with hand = newSet; boardTiles = updateBoard; tilesLeft = tilesLeft}// This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let updateBoard = List.fold(fun x (coord,(_,(c,_)))-> Map.add coord c x) st.boardTiles ms
                let playerTurn = st.playerTurn % st.numPlayers + 1u
                let timesPassed = 0u
             
                let st' = {st with boardTiles = updateBoard; playerTurn = playerTurn; timesPassed = timesPassed}// This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM (CMPassed (playerId)) ->  //Ændrer turen                
                let playerTurn = st.playerTurn % st.numPlayers + 1u
                let timesPassed = st.timesPassed + 1u
                let st' = {st with playerTurn = playerTurn; timesPassed = timesPassed}
                aux st'
            | RCM (CMChangeSuccess(newTiles)) ->
                let rec appendMultiple k v lst=
                    match v with
                    | 0u -> lst
                    | _ -> appendMultiple k (v-1u) (k :: lst)
                let lst1 : list<uint32> = List.fold(fun acc ((k : uint32), (v : uint32)) -> if v > 1u then (appendMultiple k v acc) else k :: acc ) [] newTiles                
                let newSet = List.fold(fun acc x -> MultiSet.addSingle x acc) MultiSet.empty lst1
                let tilesLeft = st.tilesLeft - uint32(List.length lst1)                
                let timesPassed = 0u
                let playerTurn = st.playerTurn % st.numPlayers + 1u
                let st' = {st with hand = newSet; timesPassed = timesPassed; playerTurn = playerTurn; tilesLeft = tilesLeft}
                aux st'
            | RCM (CMChange (playerId, numberOfTiles)) ->
                let playerTurn = st.playerTurn % st.numPlayers + 1u
                let st' = {st with playerTurn = playerTurn}
                aux st'
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st
        aux st


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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber numPlayers playerTurn handSet Map.empty 0u 100u)
        