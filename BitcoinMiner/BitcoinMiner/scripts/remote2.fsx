#time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.Remote"
printfn("hello from remote2")

open System
open System.Security.Cryptography
open System.Text
open Akka.Actor
open Akka.FSharp
open Akka.Configuration
open System.Diagnostics

type MiningInput = string*string*int*int*int
type FirstContact = string*int*list<IActorRef>
type MinedBitcoin = string*string

// remote server configuration
let configuration = 
    ConfigurationFactory.ParseString(
        @"akka {
            actor {
                provider = ""Akka.Remote.RemoteActorRefProvider, Akka.Remote""
                debug : {
                    receive : on
                    autoreceive : on
                    lifecycle : on
                    event-stream : on
                    unhandled : on
                }
            }
            remote {
                helios.tcp {
                    port = 8080
                    hostname = 127.0.0.1
                }
            }
        }")

let system = ActorSystem.Create("RemoteFSharp", configuration)

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

type Worker(name) =
    inherit Actor()
        override this.OnReceive message = 
            match message with
            | :? MiningInput as message ->
                let (existingHash, pref, difficulty, minN, maxN) = unbox<MiningInput> message
                for i = minN to maxN do
                    let newStr = pref + existingHash + i.ToString()
                    if checkBitcoin newStr difficulty then 
                        let minedhash = StringToHash newStr
                        printfn "%s %s " newStr minedhash
                        this.Sender <! (newStr, minedhash)

let mutable numRemoteActors = 0
let mutable remoteActors =[]
let previousHash = StringToHash "bathlasaksham"
let prefix = "bathlasaksham"
let st = 1
let mutable ed = 10000000
let numOfActors = Environment.ProcessorCount
let task = (ed - st)/(numOfActors)
let numZeros = fsi.CommandLineArgs.[1] |> int

let serverSupervisor = 
    spawn system "server"
    <| fun mailbox ->
        let rec loop() =             
            actor {
                let! msg = mailbox.Receive()
                let sender = mailbox.Sender()
                match box msg with
                | :? MiningInput as message ->
                    let (existingHash, pref, difficulty, minN, maxN) = unbox<MiningInput> message
                    let taskSize = (maxN - minN)/(numOfActors)//+numRemoteActor)
                    for i=1 to numOfActors do
                        system.ActorOf(Props(typedefof<Worker>, [| string(i) :> obj |])) <! (existingHash, pref, difficulty, (i-1)*taskSize+1, i*taskSize)
                
                // Handling of the message from client
                // If the client sends list of actorreferences, we spawn new unique noonces ans send the tasks to the remote workers 
                | :? FirstContact as message -> 
                    let (clientStr, numWorkers, actors) = unbox<FirstContact> message
                    numRemoteActors <- numWorkers
                    remoteActors <- remoteActors |> List.append actors
                    let newEd = ed
                    ed <- ed + task*numRemoteActors
                    if numRemoteActors > 0 then
                        for i=1 to numRemoteActors do
                            let remoteHash = previousHash + (newEd |> string)
                            remoteActors.Item(i-1) <! (remoteHash, prefix, numZeros, (i-1)*task+1, i*task)
                
                // Bitcoins found at the remote servers
                | :? MinedBitcoin as message ->
                    let (str, hash) = unbox<MinedBitcoin> message
                    printfn "%s %s " str hash    
                return! loop() 
            }
        loop()


serverSupervisor <! (previousHash, prefix, numZeros, st, ed)

Console.ReadLine()|>ignore
system.Terminate()
system.WhenTerminated.Wait()

