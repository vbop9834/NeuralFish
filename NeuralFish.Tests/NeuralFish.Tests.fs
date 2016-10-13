module NeuralFish.Tests

open NUnit.Framework
open FsUnit
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

[<Test>]
let ``When the Sensor receives the sync message, the neural circuit should activate causing the actuator to output some value`` () =
  let (testHook, testHookMailbox) = testHook ()

  let actuator = createNeuronInstance <| createActuator testHook 0
  let neuron =
    let activationFunction = fun x -> x
    let bias = 10.0
    createNeuron activationFunction 0 bias
    |> connectNodeToActuator actuator
    |> createNeuronInstance
  let sensor =
    let syncFunction =
        let data =
          [1.0; 1.0; 1.0; 1.0; 1.0]
          |> List.toSeq
        fakeDataGenerator([data])
    let weights =
      [20.0; 20.0; 20.0; 20.0; 20.0]
      |> List.toSeq
    createSensor syncFunction 0
    |> connectSensorToNode weights neuron
    |> createNeuronInstance

  Sync |> sensor.Post
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 110)

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 1

[<Test>]
let ``The NeuralFish should be able to solve the XNOR problem with predefined weights`` () =
  //(class.coursera.org/ml/lecture/48)
  let (testHook, testHookMailbox) = testHook ()

  let actuator = createNeuronInstance <| createActuator testHook 0
  let neuron_a3_1 =
    let activationFunction = sigmoid
    let bias = -10.0
    createNeuron activationFunction 0 bias
    |> connectNodeToActuator actuator
    |> createNeuronInstance
  let neuron_a2_2 =
    let activationFunction = sigmoid
    let bias = 10.0
    createNeuron activationFunction 0 bias
    |> connectNodeToNeuron 20.0 neuron_a3_1
    |> createNeuronInstance
  let neuron_a2_1 =
    let activationFunction = sigmoid
    let bias = -30.0
    createNeuron activationFunction 0 bias
    |> connectNodeToNeuron 20.0 neuron_a3_1
    |> createNeuronInstance
  let sensor_x1 =
    let syncFunction = fakeDataGenerator([[0.0]; [0.0]; [1.0]; [1.0]])
    createSensor syncFunction 0
    |> connectNodeToNeuron 20.0 neuron_a2_1
    |> connectNodeToNeuron -20.0 neuron_a2_2
    |> createNeuronInstance
  let sensor_x2 =
    let syncFunction = fakeDataGenerator([[0.0]; [1.0]; [0.0]; [1.0]])
    createSensor syncFunction 0
    |> connectNodeToNeuron 20.0 neuron_a2_1
    |> connectNodeToNeuron -20.0 neuron_a2_2
    |> createNeuronInstance

  Sync |> sensor_x1.Post
  Sync |> sensor_x2.Post
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.99))

  Sync |> sensor_x1.Post
  Sync |> sensor_x2.Post
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (lessThan 0.01))

  Sync |> sensor_x1.Post
  Sync |> sensor_x2.Post
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (lessThan 0.01))

  Sync |> sensor_x1.Post
  Sync |> sensor_x2.Post
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.99))

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 4
