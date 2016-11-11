module NeuralFish.Tests.TestHelper

open NeuralFish.Core
open NeuralFish.Types

type GeneratorMsg =
  | GetData of AsyncReplyChannel<float seq>
  | Die

let fakeDataGenerator (buffer : (float seq) list) =
  let generator = MailboxProcessor<GeneratorMsg>.Start(fun inbox ->
    let rec loop buffer =
      async {
        let! msg = inbox.Receive ()
        match msg with
        | GetData replyChannel ->
          let data = buffer |> List.head
          data |> replyChannel.Reply
          let newBuffer = (data :: (buffer |> List.tail |> List.rev)) |> List.rev
          return! loop newBuffer
        | Die ->
          return ()
      }
    loop buffer
  )
  (fun () -> GetData |> generator.PostAndReply)

type TestHookMsg =
  | SendDataToBuffer of float
  | WaitForData of AsyncReplyChannel<float>
  | Die of AsyncReplyChannel<unit>


let getTestHook () =
  let generator = MailboxProcessor<TestHookMsg>.Start(fun inbox ->
    let rec loop dataBuffer replybuffer =
      async {
        let! msg = inbox.Receive ()
        match msg with
        | SendDataToBuffer dataValue ->
          if (replybuffer |> List.isEmpty) then
            let dataBuffer = dataValue :: dataBuffer
            return! loop dataBuffer List.empty
          else
            replybuffer |> List.iter (fun (replyChannel : AsyncReplyChannel<float>) -> replyChannel.Reply dataValue)
            return! loop dataBuffer List.empty
        | Die replyChannel ->
          replyChannel.Reply () 
          replybuffer |> List.iter (fun (replyChannel : AsyncReplyChannel<float>) -> replyChannel.Reply 0.0)
          return ()
        | WaitForData replyChannel ->
          if (dataBuffer |> List.isEmpty) then
            let replybuffer = replyChannel :: replybuffer
            return! loop dataBuffer replybuffer
          else
            let dataValue =  dataBuffer |> List.head
            dataValue |> replyChannel.Reply
            replybuffer |> List.iter (fun (replyChannel : AsyncReplyChannel<float>) -> replyChannel.Reply dataValue)
            let dataBuffer = dataBuffer |> List.tail
            return! loop dataBuffer replybuffer
      }
    loop [] []
  )

  let hookFunction = (fun data -> SendDataToBuffer data |> generator.Post)
  (hookFunction, generator)

let sigmoid = (fun x -> 1.0 / (1.0 + exp(-x)))

type NeuronIdGeneratorMsg =
  | GetIntId of AsyncReplyChannel<int>

let getNumberGenerator () =
  let generator = MailboxProcessor<NeuronIdGeneratorMsg>.Start(fun inbox ->
    let rec loop currentNumber =
      async {
        let! msg = inbox.Receive ()
        match msg with
        | GetIntId replyChannel ->
          currentNumber |> replyChannel.Reply
          return! loop (currentNumber+1)
      }
    loop 0
  )
  (fun () -> GetIntId |> generator.PostAndReply)


let addNeuronToMap (neuronId, neuronInstance) =
  Map.add neuronId neuronInstance
