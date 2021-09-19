// Learn more about F# at http://fsharp.org

open System
open System.Security.Cryptography
open System.Text
open Akka.Actor
open Akka.FSharp
open System.Diagnostics


let system = System.create "BitcoinMiner" <| Configuration.defaultConfig()

type MiningInput = string*string*int*int*int

let StringToHash = fun (mystring: string) ->
    let crypt = SHA256Managed.Create()
    let myhash = StringBuilder()
    let crypto = crypt.ComputeHash(System.Text.Encoding.UTF8.GetBytes(mystring))
    for currByte in crypto do
        myhash.Append(currByte.ToString("x2"))
    myhash.ToString()
    

let checkBitcoin = fun (mystring: string) (numzeroes: int) ->
    let myHash = StringToHash(mystring)
    let mutable continueLooping = true
    let mutable counter = 0
    while continueLooping do
        let currChar = myHash.Chars(counter)
        if currChar = '0' && counter < myHash.Length 
        then counter <- counter + 1
        else continueLooping <- false
    (counter >= numzeroes)

let mine = fun (existingHash: string) (pref: string) (difficulty: int) (minNonce: int) (maxNonce: int) ->
    for i = minNonce to maxNonce do
        let newStr = pref + existingHash + i.ToString()
        if checkBitcoin newStr difficulty then 
            let minedhash = StringToHash newStr
            printfn "%s %s" newStr minedhash

type Worker(name) =
    inherit Actor()
        override this.OnReceive message = 
            match message with
            | :? MiningInput as input ->
                let (existingHash, pref, difficulty, maxN, minN) = unbox<MiningInput> input
                mine existingHash pref difficulty maxN minN
            | _ -> failwith "invalid message passed to the worker"

[<EntryPoint>]
let main argv =
    let numZeros:int = argv.[0] |> int
    let pcount = Environment.ProcessorCount
    let inithash = StringToHash "bathlasaksham"
    let pref = "bathlasaksham"
    let numOfActors = pcount
    let max = 10000000
    let taskSize = max/numOfActors
    let proc = Process.GetCurrentProcess()
    let cpuTimeStamp = proc.TotalProcessorTime
    let timer = Stopwatch()
    timer.Start()
    for i in 1..numOfActors do
        let myActor = system.ActorOf(Props(typedefof<Worker>, [| string(id) :> obj |])) //<! (inithash,pref,numZeros,(i-1)*taskSize+1, i*taskSize)
        for j in 1..100 do
            myActor <! (inithash,pref,numZeros,(i-1)*taskSize+1 + (j-1)*taskSize/100 , i*taskSize + j*taskSize/100)
    //System.Console.ReadLine() |> ignore
    system.Terminate()
    system.WhenTerminated.Wait()
    let cpuTime = (proc.TotalProcessorTime-cpuTimeStamp).TotalMilliseconds
    printfn "CPU time = %dms" (int64 cpuTime)
    printfn "Absolute time = %dms" timer.ElapsedMilliseconds
    let ratio = float (int64 cpuTime)/ float timer.ElapsedMilliseconds
    printfn "ratio: %f"  ratio
    0 // return an integer exit code