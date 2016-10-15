module NeuralFish.Tests.Exporter

open NUnit.Framework
open FsUnit
open NeuralFish.Core
open NeuralFish.Types
open NeuralFish.Exporter

open NeuralFish.Tests.TestHelper

let assertNodeRecordsContainsNode (nodeRecords : Map<int, NodeRecord>) (neuronId, (liveNeuron : NeuronInstance)) =
  let neuronType,outboundConnections = GetNeuronTypeAndOutboundConnections |> liveNeuron.PostAndReply
  let getNodeRecord nodeId = nodeRecords |> Map.find nodeId
  let assertRecordConnectionIsIdenticalTo  (nodeRecordConnections : NodeRecordConnections) (nodeId, weight) =
    nodeRecordConnections
    |> should contain (nodeId, weight)

  match neuronType with
    | NeuronType.Neuron props ->
      let nodeRecord =
        props.id
        |> getNodeRecord

      nodeRecord.ActivationFunctionId |> should not' (equal None)
      nodeRecord.ActivationFunctionId.Value |> should equal props.activationFunctionId
      nodeRecord.Bias |>  should not' (equal None)
      nodeRecord.Bias.Value |> should equal props.bias
      nodeRecord.NodeType |> should equal NodeRecordType.Neuron

      outboundConnections
      |> Seq.iter (assertRecordConnectionIsIdenticalTo nodeRecord.OutboundConnections)

    | NeuronType.Sensor props ->
      let nodeRecord =
        props.id
        |> getNodeRecord

      nodeRecord.ActivationFunctionId |> should equal Option.None
      nodeRecord.Bias |> should equal Option.None
      nodeRecord.NodeType |> should equal NodeRecordType.Sensor

      outboundConnections
      |> Seq.iter (assertRecordConnectionIsIdenticalTo nodeRecord.OutboundConnections)
    | NeuronType.Actuator props ->
      let nodeRecord =
        props.id
        |> getNodeRecord

      nodeRecord.ActivationFunctionId |> should equal Option.None
      nodeRecord.Bias |> should equal Option.None
      nodeRecord.NodeType |> should equal NodeRecordType.Actuator
      nodeRecord.OutboundConnections |> Seq.isEmpty |> should equal true

[<Test; Timeout(5000)>]
let ``Should be able to export a simple neural network to a map of node records`` () =
  let testHook = (fun x -> printfn "Actuator output %f" x)
  let getNodeId = getNumberGenerator()
  let getNeuronConnectionId = getNumberGenerator()
  let syncFunctionId = 9001
  let outputHookId = 9000
  let activationFunctionId = 777

  let actuatorId = getNodeId()
  let actuator =
      createActuator actuatorId testHook outputHookId
      |> createNeuronInstance

  let neuronId = getNodeId()
  let neuron =
    let activationFunction = id
    let bias = 10.0
    createNeuron neuronId activationFunction activationFunctionId bias
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
    let addNeuronToMap (neuronId, neuronInstance) =
      Map.add neuronId neuronInstance
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords

  actuator |> assertNodeRecordsContainsNode nodeRecords
  neuron |> assertNodeRecordsContainsNode nodeRecords
  sensor |> assertNodeRecordsContainsNode nodeRecords

[<Test; Timeout(5000)>]
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
    createActuator actuatorId testHook outputHookId
    |> createNeuronInstance
  let neuron =
    let bias = 10.0
    let id = getNodeId()
    createNeuron id activationFunction activationFunctionId bias
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
  |> (should equal 110)


  //create Node records
  let nodeRecords =
    let addNeuronToMap (neuronId, neuronInstance) =
      Map.add neuronId neuronInstance
    Map.empty
    |> addNeuronToMap actuator
    |> addNeuronToMap neuron
    |> addNeuronToMap sensor
    |> constructNodeRecords

  //Create Neural Network from Node Records
  actuator |> assertNodeRecordsContainsNode nodeRecords
  neuron |> assertNodeRecordsContainsNode nodeRecords
  sensor |> assertNodeRecordsContainsNode nodeRecords

  let activationFunctions : Map<ActivationFunctionId,ActivationFunction> =
    Map.empty
    |> Map.add activationFunctionId activationFunction
  let syncFunctions =
    Map.empty
    |> Map.add syncFunctionId syncFunction
  let outputHooks =
    Map.empty
    |> Map.add outputHookId testHook

  let sensor =
    let sensorId = (fst sensor)
    (sensorId,
     nodeRecords
     |> constructNeuralNetwork activationFunctions syncFunctions outputHooks
     |> Map.find sensorId)

  synchronize sensor

  WaitForData
  |> testHookMailbox.PostAndReply
  |> (should equal 110)

  let testAssertionCount = Die |> testHookMailbox.PostAndReply

  testAssertionCount |> should equal 2
