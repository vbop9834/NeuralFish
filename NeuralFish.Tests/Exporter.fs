module NeuralFish.Tests.Exporter

open NUnit.Framework
open FsUnit
open NeuralFish.Core
open NeuralFish.Types
open NeuralFish.Exporter

let assertNodeRecordsContainsNode (nodeRecords : Map<int, NodeRecord>) neuronType =
  let getNodeRecord nodeId = nodeRecords |> Map.find nodeId
  let assertRecordConnectionIsIdenticalTo  (nodeRecordConnections : NodeRecordConnections) (nodeConnection : NeuronConnection) =
    nodeRecordConnections |> Map.containsKey nodeConnection.nodeId |> should equal true
    nodeRecordConnections
    |> Map.find nodeConnection.nodeId
    |> (fun weight -> weight |> should equal nodeConnection.weight)

  match neuronType with
    | NeuronType.Neuron props ->
      let nodeRecord =
        props.id
        |> getNodeRecord

      nodeRecord.ActivationFunctionId |> Option.isSome |> should equal true
      nodeRecord.ActivationFunctionId.Value |> should equal props.activationFunctionId
      nodeRecord.Bias |> Option.isSome |> should equal true
      nodeRecord.Bias.Value |> should equal props.bias
      nodeRecord.NodeType |> should equal NodeRecordType.Neuron

      props.outbound_connections
      |> Seq.iter (assertRecordConnectionIsIdenticalTo nodeRecord.OutboundConnections)

    | NeuronType.Sensor props ->
      let nodeRecord =
        props.id
        |> getNodeRecord

      nodeRecord.ActivationFunctionId |> should equal Option.None
      nodeRecord.Bias |> should equal Option.None
      nodeRecord.NodeType |> should equal NodeRecordType.Sensor

      props.outbound_connections
      |> Seq.iter (assertRecordConnectionIsIdenticalTo nodeRecord.OutboundConnections)
    | NeuronType.Actuator props ->
      let nodeRecord =
        props.id
        |> getNodeRecord

      nodeRecord.ActivationFunctionId |> should equal Option.None
      nodeRecord.Bias |> should equal Option.None
      nodeRecord.NodeType |> should equal NodeRecordType.Actuator
      nodeRecord.OutboundConnections |> Map.isEmpty |> should equal true

[<Test>]
let ``Should be able to export a simple neural network to a map of node records`` () =
  let testHook = (fun x -> printfn "Actuator output %f" x)
  let syncFunctionId = 9001
  let testHookId = 9000
  let (actuatorProps, actuator) =
      createActuator testHook testHookId
      |> createNeuronInstance
  let (neuronProps, neuron) =
    let activationFunction = fun x -> x
    let bias = 10.0
    createNeuron activationFunction 0 bias
    |> connectNodeToActuator actuator
    |> createNeuronInstance
  let (sensorProps, sensor) =
    let syncFunction = (fun () -> Seq.empty)
    let weights =
      [20.0; 20.0; 20.0; 20.0; 20.0]
      |> List.toSeq
    createSensor syncFunction 0
    |> connectSensorToNode weights neuron
    |> createNeuronInstance

  let nodes = [ actuatorProps; neuronProps; sensorProps ]
  let nodeRecords =
    nodes
    |> List.toSeq
    |> buildFlatNodeList
  nodes |> List.iter (assertNodeRecordsContainsNode nodeRecords)
