module NeuralFish.Tests

open NUnit.Framework
open FsUnit
open NeuralFish

type GeneratorMsg =
  | GetData of AsyncReplyChannel<float>
  | Die

let fakeSensorData dataVector =
  let generator = MailboxProcessor<GeneratorMsg>.Start(fun inbox ->
    let rec loop dataVector =
      async {
        let! msg = inbox.Receive ()
        let getData dataVector =
          if dataVector |> List.isEmpty then
            0.0
          else
          dataVector |> List.head

        match msg with
        | GetData replyChannel ->
          getData dataVector
          |> replyChannel.Reply
          return! loop dataVector
        | Die ->
          return ()
      }
    loop dataVector
  )
  (fun () -> GetData |> generator.PostAndReply)

let sigmoid = (fun x -> 1.0 / (1.0 + exp(-1.0 * x)))

[<Test>]
let ``test`` () =
  let assertion (actuatorOutput : float) =
    actuatorOutput |> should be (greaterThan 0.0)

  let actuator = createNeuronInstance <| createActuator assertion
  let neuron =
    let activationFunction = sigmoid
    let bias = 10.0
    createNeuron activationFunction bias
    |> connectNodeTo actuator
    |> createNeuronInstance
  let sensor =
    let syncFunction = fakeSensorData(List.empty)
    createSensor syncFunction
    |> connectNodeToNeuron 20.0 neuron
    |> createNeuronInstance

  Sync |> sensor.Post
