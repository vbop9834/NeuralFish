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
        let getData buffer =
          if buffer |> List.isEmpty then
            Seq.empty
          else
            buffer |> List.head
        match msg with
        | GetData replyChannel ->
          getData buffer
          |> replyChannel.Reply
          return! loop (buffer |> List.tail)
        | Die ->
          return ()
      }
    loop buffer
  )
  (fun () -> GetData |> generator.PostAndReply)

type TestHookMsg =
  | SendDataToBuffer of float
  | WaitForData of AsyncReplyChannel<float>
  | Die of AsyncReplyChannel<int>

// let testHook x = printfn "%A" x

let testHook () =
  let generator = MailboxProcessor<TestHookMsg>.Start(fun inbox ->
    let rec loop dataBuffer counter replybuffer =
      async {
        let! msg = inbox.Receive ()
        match msg with
        | SendDataToBuffer dataValue ->
          if (replybuffer |> List.isEmpty) then
            let dataBuffer = dataValue :: dataBuffer
            return! loop dataBuffer counter List.empty
          else
            let counter = counter + 1
            replybuffer |> List.iter (fun (replyChannel : AsyncReplyChannel<float>) -> replyChannel.Reply dataValue)
            return! loop dataBuffer counter List.empty
        | Die replyChannel ->
          replyChannel.Reply counter
          replybuffer |> List.iter (fun (replyChannel : AsyncReplyChannel<float>) -> replyChannel.Reply 0.0)
          return ()
        | WaitForData replyChannel ->
          if (dataBuffer |> List.isEmpty) then
            let replybuffer = replyChannel :: replybuffer
            return! loop dataBuffer counter replybuffer
          else
            let dataValue =  dataBuffer |> List.head
            dataValue |> replyChannel.Reply
            replybuffer |> List.iter (fun (replyChannel : AsyncReplyChannel<float>) -> replyChannel.Reply dataValue)
            let dataBuffer = dataBuffer |> List.tail
            return! loop dataBuffer counter replybuffer
      }
    loop [] 0 []
  )

  let hookFunction = (fun data -> SendDataToBuffer data |> generator.Post)
  (hookFunction, generator)

let sigmoid = (fun x -> 1.0 / (1.0 + exp(-x)))
