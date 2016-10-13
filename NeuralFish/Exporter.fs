module NeuralFish.Exporter

open NeuralFish.Types

type NodeConnection =
  {
    NodeId: int
    Weight: float
  }

type NodeRecord =
  {
    NodeType: NeuronType
    OutputConnections: NodeConnection seq
    Bias: float option
    ActivationFunctionId: int option
    SyncFunctionId: int option
    OutputHookId: int option
  }

let buildFlatNodeList nodeList =
  let generateNodeRecord neuronType =
    let createConnectionsRecord outboundConnections =
      outboundConnections
      |> Seq.map(fun connection -> { NodeId = connection.nodeId; Weight = connection.weight })
    match neuronType with
    | Neuron props ->
      let connections = props.outbound_connections |> createConnectionsRecord
      {
        NodeType = neuronType
        OutputConnections = connections
        Bias = Some props.bias
        ActivationFunctionId = Some props.activationFunctionId
        SyncFunctionId = None
        OutputHookId = None
      }
    | Actuator props ->
      {
        NodeType = neuronType
        OutputConnections = Seq.empty
        Bias = None
        ActivationFunctionId = None
        SyncFunctionId = None
        OutputHookId = Some props.outputHookId
      }
    | Sensor props ->
      let connections = props.outbound_connections |> createConnectionsRecord
      {
        NodeType = neuronType
        OutputConnections = connections
        Bias = None
        ActivationFunctionId = None
        SyncFunctionId = Some props.syncFunctionId
        OutputHookId = None
      }

  nodeList
  |> Seq.map generateNodeRecord
