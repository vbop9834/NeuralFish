module NeuralFish.Tests.Exporter

open Xunit
open FsUnit
open FsUnit.Xunit

open NeuralFish.Core
open NeuralFish.Types
open NeuralFish.Exporter

open NeuralFish.Tests.TestHelper

let assertNodeRecordsContainsNode (nodeRecords : NodeRecords) (neuronId, (_, liveNeuron : NeuronInstance)) =
  let liveNeuronNodeRecord = GetNodeRecord |> liveNeuron.PostAndReply
  let getNodeRecord nodeId = nodeRecords |> Map.find nodeId
  let assertRecordConnectionIsIdenticalTo  (nodeRecordConnections : NodeRecordConnections)  =
    (fun nodeRecordConnectionId (nodeId, weight) ->
      nodeRecordConnections
      |> Map.containsKey nodeRecordConnectionId
      |> should equal true

      let (generatedTargetNodeId, generatedWeight) = nodeRecordConnections |> Map.find nodeRecordConnectionId
      generatedWeight |> should equal weight
      generatedTargetNodeId |> should equal nodeId
    )

  match liveNeuronNodeRecord.NodeType with
    | NodeRecordType.Neuron ->
      let nodeRecord =
        liveNeuronNodeRecord.NodeId
        |> getNodeRecord

      nodeRecord.ActivationFunctionId |> should not' (equal None)
      nodeRecord.ActivationFunctionId.Value |> should equal liveNeuronNodeRecord.ActivationFunctionId.Value
      nodeRecord.Bias |>  should not' (equal None)
      nodeRecord.Bias.Value |> should equal liveNeuronNodeRecord.Bias.Value
      nodeRecord.NodeType |> should equal NodeRecordType.Neuron
      nodeRecord.Layer |> should equal liveNeuronNodeRecord.Layer

      liveNeuronNodeRecord.OutboundConnections
      |> Map.iter (assertRecordConnectionIsIdenticalTo nodeRecord.OutboundConnections)

    | NodeRecordType.Sensor ->
      let nodeRecord =
        liveNeuronNodeRecord.NodeId
        |> getNodeRecord

      nodeRecord.ActivationFunctionId |> should equal Option.None
      nodeRecord.Bias |> should equal Option.None
      nodeRecord.NodeType |> should equal NodeRecordType.Sensor
      nodeRecord.Layer |> should equal 0.0

      liveNeuronNodeRecord.OutboundConnections
      |> Map.iter (assertRecordConnectionIsIdenticalTo nodeRecord.OutboundConnections)
    | NodeRecordType.Actuator ->
      let nodeRecord =
        liveNeuronNodeRecord.NodeId
        |> getNodeRecord

      nodeRecord.ActivationFunctionId |> should equal Option.None
      nodeRecord.Bias |> should equal Option.None
      nodeRecord.NodeType |> should equal NodeRecordType.Actuator
      nodeRecord.OutboundConnections |> Seq.isEmpty |> should equal true
      nodeRecord.Layer |> should equal liveNeuronNodeRecord.Layer

[<Fact>]
let ``Should be able to export a simple neural network to a map of node records`` () =
  let testHook = (fun x -> printfn "Actuator output %f" x)
  let getNodeId = getNumberGenerator()
  let getNeuronConnectionId = getNumberGenerator()
  let syncFunctionId = 9001
  let outputHookId = 9000
  let activationFunctionId = 777

  let actuatorId = getNodeId()
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance

  let neuronId = getNodeId()
  let neuron =
    let activationFunction = id
    let bias = 10.0
    let layer = 2.0
    createNeuron neuronId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensorId = getNodeId()
  let syncFunction = (fun () -> Seq.empty)
  let sensor =
    createSensor sensorId syncFunction syncFunctionId
    |> createNeuronInstance

  neuron |> connectNodeToActuator actuator

  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq
  sensor |> connectSensorToNode neuron weights

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords

  actuator |> assertNodeRecordsContainsNode nodeRecords
  neuron |> assertNodeRecordsContainsNode nodeRecords
  sensor |> assertNodeRecordsContainsNode nodeRecords

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

[<Fact>]
let ``Should be able to construct a simple neural network from a map of node records`` () =
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()

  let syncFunction =
      let data =
        [1.0; 1.0; 1.0; 1.0; 1.0]
        |> List.toSeq
      fakeDataGenerator([data;data])
  let syncFunctionId = 9001

  let outputHookId = 9000
  let activationFunction = id
  let activationFunctionId = 777
  let actuatorId = getNodeId()

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 10.0
    let id = getNodeId()
    let layer = 2.0
    createNeuron id layer activationFunction activationFunctionId bias
    |> createNeuronInstance
  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq

  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToActuator actuator

  //test Neural network
  synchronize sensor

  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 110.0)


  //create Node records
  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords

  //Create Neural Network from Node Records
  actuator |> assertNodeRecordsContainsNode nodeRecords
  neuron |> assertNodeRecordsContainsNode nodeRecords
  sensor |> assertNodeRecordsContainsNode nodeRecords

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let neuralNetwork =
   nodeRecords
   |> constructNeuralNetwork activationFunctions syncFunctions outputHooks

  let sensor =
    let sensorId = (fst sensor)
    let layer = 1.0
    (sensorId,
     neuralNetwork
     |> Map.find sensorId)

  synchronize sensor

  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 110.0)

  neuralNetwork
  |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``Should be able to solve the XNOR problem with predefined weights, convert Neural Network to records, then restore the network and get the same results`` () =
  //(class.coursera.org/ml/lecture/48)
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()

  let syncFunction_x1 = fakeDataGenerator([[0.0; 0.0]; [0.0; 0.0]; [1.0; 1.0]; [1.0; 1.0]])
  let syncFunctionId_x1 = 0

  let syncFunction_x2 = fakeDataGenerator([[0.0; 0.0]; [1.0; 1.0]; [0.0; 0.0]; [1.0; 1.0]])
  let syncFunctionId_x2 = 1

  let activationFunction = sigmoid
  let activationFunctionId = 0

  let outputHookId = 0

  let actuator =
    let id = getNodeId()
    let layer = 4.0
    createNeuronInstance <| createActuator id layer testHook outputHookId
  let neuron_a3_1 =
    let bias = -10.0
    let id = getNodeId()
    let layer = 3.0
    createNeuron id layer activationFunction 0 bias
    |> createNeuronInstance
  let neuron_a2_2 =
    let bias = 10.0
    let id = getNodeId()
    let layer = 2.0
    createNeuron id layer activationFunction activationFunctionId bias
    |> createNeuronInstance
  let neuron_a2_1 =
    let bias = -30.0
    let id = getNodeId()
    let layer = 2.0
    createNeuron id layer activationFunction activationFunctionId bias
    |> createNeuronInstance
  let sensor_x1 =
    let id = getNodeId()
    createSensor id syncFunction_x1 syncFunctionId_x1
    |> createNeuronInstance
  let sensor_x2 =
    let id = getNodeId()
    createSensor id syncFunction_x2 syncFunctionId_x2
    |> createNeuronInstance

  let weightOne = 20.0
  let weightTwo = -20.0
  sensor_x1 |> connectNodeToNeuron neuron_a2_1 weightOne
  sensor_x1 |> connectNodeToNeuron neuron_a2_2 weightTwo
  sensor_x2 |> connectNodeToNeuron neuron_a2_1 weightOne
  sensor_x2 |> connectNodeToNeuron neuron_a2_2 weightTwo

  let weight = 20.0
  neuron_a2_1 |> connectNodeToNeuron neuron_a3_1 weight
  neuron_a2_2 |> connectNodeToNeuron neuron_a3_1 weight

  neuron_a3_1 |> connectNodeToActuator actuator

  synchronize sensor_x1
  synchronize sensor_x2
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.99))

  synchronize sensor_x1
  synchronize sensor_x2
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (lessThan 0.01))

  synchronize sensor_x1
  synchronize sensor_x2
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (lessThan 0.01))

  synchronize sensor_x1
  synchronize sensor_x2
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.99))

  //create Node records
  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron_a2_1
    |> addNeuronToMap neuron_a2_2
    |> addNeuronToMap neuron_a3_1
    |> addNeuronToMap sensor_x1
    |> addNeuronToMap sensor_x2
    |> constructNodeRecords

  //Create Neural Network from Node Records
  actuator |> assertNodeRecordsContainsNode nodeRecords
  neuron_a2_1 |> assertNodeRecordsContainsNode nodeRecords
  neuron_a2_2 |> assertNodeRecordsContainsNode nodeRecords
  neuron_a3_1 |> assertNodeRecordsContainsNode nodeRecords
  sensor_x1 |> assertNodeRecordsContainsNode nodeRecords
  sensor_x2 |> assertNodeRecordsContainsNode nodeRecords

  let activationFunctions : Map<ActivationFunctionId,ActivationFunction> =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId_x1 syncFunction_x1
    |> Map.add syncFunctionId_x2 syncFunction_x2
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let neuralNetwork =
    nodeRecords
    |> constructNeuralNetwork activationFunctions syncFunctions outputHooks

  let sensorX1, sensorX2 =
    let sensorIdX1 =
      let id = (fst sensor_x1)
      (id,1, neuralNetwork|> Map.find id)
    let sensorIdX2 =
      let id = (fst sensor_x2)
      (id,1, neuralNetwork|> Map.find id)
    (sensor_x1, sensor_x2)

  synchronize sensorX1
  synchronize sensorX2
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.99))

  synchronize sensorX1
  synchronize sensorX2
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (lessThan 0.01))

  synchronize sensorX1
  synchronize sensorX2
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (lessThan 0.01))

  synchronize sensorX1
  synchronize sensorX2
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should be (greaterThan 0.99))

  [
    sensor_x1
    sensor_x2
    neuron_a2_1
    neuron_a2_2
    neuron_a3_1
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``Should be able to export a recurrent neural network to a map of node records`` () =
  let testHook = (fun x -> printfn "Actuator output %f" x)
  let getNodeId = getNumberGenerator()
  let getNeuronConnectionId = getNumberGenerator()
  let syncFunctionId = 9001
  let outputHookId = 9000
  let activationFunctionId = 777

  let actuatorId = getNodeId()
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance

  let neuronId = getNodeId()
  let neuron =
    let activationFunction = id
    let bias = 10.0
    let layer = 2.0
    createNeuron neuronId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensorId = getNodeId()
  let syncFunction = (fun () -> Seq.empty)
  let sensor =
    createSensor sensorId syncFunction syncFunctionId
    |> createNeuronInstance

  neuron |> connectNodeToActuator actuator

  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq
  sensor |> connectSensorToNode neuron weights
  neuron |> connectNodeToNeuron neuron 20.0

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords

  actuator |> assertNodeRecordsContainsNode nodeRecords
  neuron |> assertNodeRecordsContainsNode nodeRecords
  sensor |> assertNodeRecordsContainsNode nodeRecords

  [
    sensor
    neuron
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

[<Fact>]
let ``Should be able to deconstruct then reconstruct recurrent neural network with three neurons`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001
  let syncFunctionId = 0
  let activationFunctionId = 0
  let activationFunction = id
  let syncFunction =
    let data =
      [1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0; 1.0]
      |> List.toSeq
    fakeDataGenerator([data])

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron_1a =
    let bias = 10.0
    let nodeId = getNodeId()
    let layer = 1.0
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let neuron_1b =
    let bias = 10.0
    let nodeId = getNodeId()
    let layer = 1.0
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let neuron_2a =
    let bias = 10.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let nodeId = getNodeId()
    createSensor nodeId syncFunction syncFunctionId
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq

  sensor |> connectSensorToNode neuron_1a weights
  neuron_1a |> connectNodeToActuator actuator
  neuron_2a |> connectNodeToActuator actuator
  neuron_1a |> connectNodeToNeuron neuron_1a 20.0
  neuron_1a |> connectNodeToNeuron neuron_1b 20.0
  neuron_1b |> connectNodeToNeuron neuron_1b 20.0
  neuron_1b |> connectNodeToNeuron neuron_2a 20.0

  //Synchronize and Assert!
  //Since there is a recurrent connection then the output will
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 44320.0)

  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 1810520.0)

  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 54734520.0)

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron_1a
    |> addNeuronToMap neuron_1b
    |> addNeuronToMap neuron_2a
    |> addNeuronToMap sensor
    |> constructNodeRecords

  actuator |> assertNodeRecordsContainsNode nodeRecords
  neuron_1a |> assertNodeRecordsContainsNode nodeRecords
  neuron_1b |> assertNodeRecordsContainsNode nodeRecords
  neuron_2a |> assertNodeRecordsContainsNode nodeRecords
  sensor |> assertNodeRecordsContainsNode nodeRecords

  [
    sensor
    neuron_1a
    neuron_1b
    neuron_2a
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let neuralNetwork =
    nodeRecords
    |> constructNeuralNetwork activationFunctions syncFunctions outputHooks

  synchronizeNN neuralNetwork
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 44320.0)

  synchronizeNN neuralNetwork
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 1810520.0)

  synchronizeNN neuralNetwork
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 54734520.0)

  neuralNetwork |> killNeuralNetwork

  Die |> testHookMailbox.PostAndReply

[<Fact>]
let ``After reconstruction, Sensor should inflate data if there is not enough data to go to all the neuron connections`` () =
  //Test setup
  let (testHook, testHookMailbox) = getTestHook ()
  let getNodeId = getNumberGenerator()
  let actuatorId = getNodeId()
  let outputHookId = 9001

  let syncFunctionId = 0
  let syncFunction =
   let data =
     [1.0; 1.0; 1.0; 1.0; 1.0]
     |> List.toSeq
   fakeDataGenerator([data])

  let activationFunctionId = 0
  let activationFunction = id

  //Create Neurons
  let actuator =
    let layer = 3.0
    createActuator actuatorId layer testHook outputHookId
    |> createNeuronInstance
  let neuron_1a =
    let bias = 10.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let neuron_1b =
    let bias = 0.0
    let nodeId = getNodeId()
    let layer = 2.0
    createNeuron nodeId layer activationFunction activationFunctionId bias
    |> createNeuronInstance

  let sensor =
    let id = getNodeId()
    createSensor id syncFunction syncFunctionId
    |> createNeuronInstance

  //Connect Neurons
  let weights =
    [
      20.0
      20.0
      20.0
      20.0
      20.0
    ] |> List.toSeq

  sensor |> connectSensorToNode neuron_1a weights
  sensor |> connectSensorToNode neuron_1b weights
  neuron_1a |> connectNodeToActuator actuator
  neuron_1b |> connectNodeToActuator actuator

  //Synchronize and Assert!
  synchronize sensor
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 110.0)

  let nodeRecords =
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron_1a
    |> addNeuronToMap neuron_1b
    |> addNeuronToMap sensor
    |> constructNodeRecords

  actuator |> assertNodeRecordsContainsNode nodeRecords
  neuron_1a |> assertNodeRecordsContainsNode nodeRecords
  neuron_1b |> assertNodeRecordsContainsNode nodeRecords
  sensor |> assertNodeRecordsContainsNode nodeRecords

  [
    sensor
    neuron_1a
    neuron_1b
    actuator
  ]
  |> Map.ofList
  |> killNeuralNetwork

  let activationFunctions : ActivationFunctions =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let neuralNetwork =
    nodeRecords
    |> constructNeuralNetwork activationFunctions syncFunctions outputHooks

  synchronizeNN neuralNetwork
  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 110.0)

  Die |> testHookMailbox.PostAndReply
