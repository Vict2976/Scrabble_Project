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
            printfn "Updated Map: %A" st.boardTiles
            
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
            
            
            
  //i findword, bogstaver vi har tilbage på hånd, der hvor vi er nået til i dict,
  //char vi har ind til videre, det ord vi har fundet so far.
            
            //CurrentWord 'H'E'Z'
            //FoundWord H'E
            
            //resterende hånd
            // acc = resternde 6 nogstaver samtidig med FoundWord - Måske tuple?
            //List.remove (elemtet som vi er kommet til) hand
            
           // acc = (i, foundWord)
            
            //Mangler: Sammenligne currentword med foundword og så retunere største ord 
            
             
            
            let rec findWord (hand: list<char>) (D : Dictionary.Dict )(currentWord : list<char>) (FoundWord : list<char>) =
                let aux (i, acc) e =
                  match Dictionary.step e D with
                  | Some (true, Drest) -> (i+1, findWord (List.removeAt i hand) Drest (e::currentWord) (e::currentWord))
                  | Some (false, Drest) -> (i+1, findWord (List.removeAt i hand) Drest (e::currentWord) acc)
                  | None -> (i, acc)
                match hand with
                | [] -> FoundWord
                | hand1 -> List.fold aux (0, FoundWord) hand1 |> snd 
                
                    
                
            //let test = findWord ['X';'H';'E';'L';'L';'O'] st.dict [] []
            
            //start of the game. If the map is empty findword from hand and play it.
            let charToIntMapAlphabet = Map.add 'A' 1u Map.empty |> Map.add 'B' 2u |> Map.add 'C' 3u |> Map.add 'D' 4u |> Map.add 'E' 5u |> Map.add 'F' 6u
                                       |> Map.add 'G' 7u|> Map.add 'H' 8u |> Map.add 'I' 9u |> Map.add 'J' 10u |> Map.add 'K' 11u |> Map.add 'L' 12u
                                       |> Map.add 'M' 13u |> Map.add 'N' 14u |> Map.add 'O' 15u |> Map.add 'P' 16u |> Map.add 'Q' 17u |> Map.add 'R' 18u
                                       |> Map.add 'S' 19u |> Map.add 'T' 20u |> Map.add 'U' 21u |> Map.add 'V' 22u |> Map.add 'W' 23u |> Map.add 'X' 24u
                                       |> Map.add 'Y' 25u |> Map.add 'Z' 26u
            
            let playFirstMove =
                if st.boardTiles.IsEmpty then List.rev (findWord chars st.dict [] []) else []
            
             
            printfn "PlayFirstMove %A" playFirstMove                
            
            //val ms: (coord * (uint32 * (char * int))) list                                                     
            let rec constructMove (chars:list<char>) (move: list<((int * int) * (uint32 * (char * int)))>) (index : (int*int)) =               
               
               let aux nyListe stadie =
                   let tile = (((snd index),0):coord),((Map.find (List.item ((fst index)-1) chars) charToIntMapAlphabet), Set.minElement (Map.find (Map.find (List.item ((fst index)-1) chars) charToIntMapAlphabet) pieces))               
                   match stadie with
                   | (i,n) -> constructMove chars (tile::nyListe) (i-1,n-1)
               
               match index with
               | (0,_) -> move
               | (i,n) -> aux move (i,n)                 
                                                                        
               
            let move = constructMove playFirstMove [] ((List.length playFirstMove),(List.length playFirstMove)-1)
            printfn "KÆMPE TEST På Move %A"  move
               
          

            //fold hen over hånden
            //
            //  MultiSet.fold (fun acc key value ->
            // find char by key
            // step into dict with the char
            // remove char from hand
            // call findWord recursively
            
           // ) [] st.hand
            
            let test4 =
                Dictionary.step 'H' st.dict
                |> Option.bind (fun (b, dict') ->
                    Dictionary.step 'E' dict' |> Option.bind (fun (b, dict') ->
                        Dictionary.step 'Y' dict'))
                                
            (*
            let test1 =
                Dictionary.step 'H' st.dict
                |> Option.bind (fun (b, dict') ->
                    Dictionary.step 'E' dict' |> Option.bind (fun (b, dict') ->
                        Dictionary.step 'Y' dict'))
            
            let test2 =
                Dictionary.step 'H' st.dict
                |> Option.bind (fun (b, dict') ->
                    Dictionary.step 'E' dict')
                
            let test3 = Dictionary.step 'H' st.dict
             
            printf "TEST H,E,Y,X: %A" test4
            printf "TEST H,E,Y : %A" test1
            printf "TEST H,E : %A" test2
            printf "TEST H: %A" test3
            *)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            
            (*let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input*)

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                //Updating hand
                let lst = List.map (fun (_, (u, _)) -> u) ms
                let deletedSet = List.fold(fun acc x -> MultiSet.removeSingle x acc) st.hand lst                                     
                let lst1 = List.map (fun (u, _) -> u) newPieces
                let newSet = List.fold(fun acc x -> MultiSet.addSingle x acc) deletedSet lst1
                
                //Updating board
                //val ms: (coord * (uint32 * (char * int))) list
                let updateBoard = List.fold(fun x (coord,(_,(c,_)))-> Map.add coord c x) st.boardTiles ms
                
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = {st with hand = newSet; boardTiles = updateBoard}// This state needs to be updated
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty)
        