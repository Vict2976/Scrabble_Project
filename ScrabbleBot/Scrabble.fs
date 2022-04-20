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
        hand          : MultiSet.MultiSet<uint32>
        boardTiles    : Map<coord, char>
        
        // Hvilke brikker ligger der?? (Map<coord, en slags tile>)
    }

    let mkState b d pn h bt = {board = b; dict = d;  playerNumber = pn; hand = h; boardTiles = bt}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let boardTiles st   = st.boardTiles
    
    //let removeFromHand ms (st : state) : state =
        //st.hand 

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            
            let rec addAmount tileSet list (amount:uint) =
                match amount with
                | 0u -> list
                | _ -> addAmount tileSet (tileSet::list) (amount-1u)
            
            let findTiles pieces hand = MultiSet.fold (fun acc id amount-> addAmount (Map.find id pieces) acc amount) [] hand
            let newHand  = findTiles pieces st.hand
            
            let chars = List.fold(fun acc set -> (Set.fold(fun acc pair -> fst pair :: acc) [] set) @ acc) [] newHand
            let pointValues = List.fold(fun acc set -> (Set.fold(fun acc pair -> snd pair :: acc) [] set) @ acc) [] newHand
            
            
            let rec add tileSet list (amount:uint) =
                match amount with
                | 0u -> list
                | _ -> add tileSet (tileSet::list) (amount-1u)
                
            let id = MultiSet.foldBack (fun id n acc -> add id acc n) st.hand []
            
            let tile : coord * (uint32 * (char * int)) = (0,0), (id.[0], (chars.[0], pointValues.[0]))
            let tileTwo : coord * (uint32 * (char * int)) = (0,1), (id.[1], (chars.[1], pointValues.[1]))
            let tileThree : coord * (uint32 * (char * int)) = (0,2), (id.[2], (chars.[2], pointValues.[2]))
            
            let word = [tile; tileTwo; tileThree]
         
            printf "WOOOOORD: %A" (string word)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let move = word

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                let lst = List.map (fun (_, (u, _)) -> u) ms
                let deletedSet = List.fold(fun acc x -> MultiSet.removeSingle x acc) st.hand lst                                     
                let lst1 = List.map (fun (u, _) -> u) newPieces
                let newSet = List.fold(fun acc x -> MultiSet.addSingle x acc) deletedSet lst1
                
               // let newSet = MultiSet.removeSingle (first) (st.hand)
                // let newSet = List.map(fun x  -> MultiSet.removeSingle (first) (st.hand))
                // newPieces: (uint32 * uint32) list. (id * antal) list
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = {st with hand = newSet}// This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option)
            (boardTiles : Map<coord, char>)
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet boardTiles)
        