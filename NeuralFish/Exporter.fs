module NeuralFish.Exporter

open NeuralFish.Types

type NodeRecordType =
  | Neuron
  | Sensor
  | Actuator

type NodeRecordConnections = Map<int,float>

type NodeRecord =
  {
    NodeId: int
    NodeType: NodeRecordType
    //connection is nodeid*weight
    OutboundConnections: NodeRecordConnections
    Bias: float option
    ActivationFunctionId: int option
    SyncFunctionId: int option
    OutputHookId: int option
  }

let buildFlatNodeList nodeList =
  let generateNodeRecord neuronType =
    let createConnectionsRecord outboundConnections =
      outboundConnections
      |> Seq.map(fun connection -> connection.nodeId, connection.weight)
      |> Map.ofSeq
    match neuronType with
    | NeuronType.Neuron props ->
      let connections = props.outbound_connections |> createConnectionsRecord
      (props.id, {
        NodeId = props.id
        NodeType = Neuron
        OutboundConnections = connections
        Bias = Some props.bias
        ActivationFunctionId = Some props.activationFunctionId
        SyncFunctionId = None
        OutputHookId = None
      })
    | NeuronType.Actuator props ->
      (props.id, {
        NodeId = props.id
        NodeType = Actuator
        OutboundConnections = Map.empty
        Bias = None
        ActivationFunctionId = None
        SyncFunctionId = None
        OutputHookId = Some props.outputHookId
      })
    | NeuronType.Sensor props ->
      let connections = props.outbound_connections |> createConnectionsRecord
      (props.id, {
        NodeId = props.id
        NodeType = Sensor
        OutboundConnections = connections
        Bias = None
        ActivationFunctionId = None
        SyncFunctionId = Some props.syncFunctionId
        OutputHookId = None
      })

  nodeList
  |> Seq.map generateNodeRecord
  |> Map.ofSeq
