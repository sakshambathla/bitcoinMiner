#time "on"
#r "nuget: Akka.FSharp" 
#r "nuget: Akka.Remote"
printfn("hello from remote1")

open System
open System.Security.Cryptography
open System.Text
open Akka.Actor
open Akka.FSharp
open System.Diagnostics
open Akka.Configuration

type MiningInput = string*string*int*int*int
type MinedBitcoin = string*string
type FirstContact = string*int*List<IActorRef>

let configuration = 
    ConfigurationFactory.ParseString(
        @"akka {
            actor {
                provider = ""Akka.Remote.RemoteActorRefProvider, Akka.Remote""
                
            }
            remote {
                helios.tcp {
                    port = 8081
                    hostname = localhost
                }
            }
        }")

let system = ActorSystem.Create("ClientFsharp", configuration)
let serverip = fsi.CommandLineArgs.[1] |> string
let addr = "akka.tcp://RemoteFSharp@" + serverip + ":8080/user/server"
let numOfActors = Environment.ProcessorCount

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
                        //printfn "%s %s " newStr minedhash
                        this.Sender <! (newStr, minedhash)
                        
            | _ -> failwith "invalid message passed to the worker"

// list of actor references to be deployed remotely
let mutable myActors = []
for i in 1..numOfActors do
    myActors <- List.append myActors [system.ActorOf(Props(typedefof<Worker>, [| string(id) :> obj |]))]

// Communication link: actor to connect to the server and send messages/ IActorReference list to remotely deploy the actor
let clientSupervisor = 
    spawn system "client"
    <| fun mailbox ->
        let rec loop() =
            actor {
                let! msg = mailbox.Receive()
                let sender = mailbox.Sender()
                let echoServer = system.ActorSelection(addr)
                match box msg with
                | :? FirstContact as message -> 
                    echoServer.Tell(message)     
                | :? MinedBitcoin as message ->
                    printfn "got a bitcoin sending to server: %A" message
                    echoServer.Tell(message)
                | _ -> failwith "invalid message passed to the supervisor"
                return! loop() 
            }
        loop()

clientSupervisor <! ("client-1", numOfActors, myActors)

Console.ReadLine()|>ignore
system.Terminate()
system.WhenTerminated.Wait()