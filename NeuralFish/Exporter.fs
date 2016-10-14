module NeuralFish.Exporter

open NeuralFish.Core
open NeuralFish.Types

type NodeRecordType =
  | Neuron
  | Sensor
  | Actuator

type NodeRecordConnections = Map<NeuronId,Weight>

type NodeRecord =
  {
    NodeId: NeuronId
    NodeType: NodeRecordType
    OutboundConnections: NodeRecordConnections
    Bias: Bias option
    ActivationFunctionId: ActivationFunctionId option
    SyncFunctionId: ActivationFunctionId option
    OutputHookId: OutputHookId option
  }

type NodeRecords = Map<NeuronId,NodeRecord>

let constructNodeRecords nodeList : NodeRecords =
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

exception NodeRecordTypeException of string
exception NeuronInstanceException of string

let constructNeuralNetwork (activationFunctions : Map<ActivationFunctionId,ActivationFunction>)  (syncFunctions : Map<SyncFunctionId,SyncFunction>) (outputHooks : Map<OutputHookId,OutputHookFunction>) (nodeRecords : NodeRecords) =
  let createNeuronFromRecord nodeId (nodeRecord : NodeRecord) =
    match nodeRecord.NodeType with
    | Neuron ->
      if (nodeRecord.ActivationFunctionId |> Option.isNone) then
        raise (NodeRecordTypeException <| sprintf "Neuron with id %A does not have a Activation function id" nodeRecord.NodeId)
      if (nodeRecord.Bias |> Option.isNone) then
        raise (NodeRecordTypeException <| sprintf "Neuron with id %A does not have a Bias" nodeRecord.NodeId)
      let activationFunction = activationFunctions |> Map.find nodeRecord.ActivationFunctionId.Value
      createNeuron nodeId activationFunction nodeRecord.ActivationFunctionId.Value nodeRecord.Bias.Value
      |> createNeuronInstance
    | Sensor ->
      if (nodeRecord.SyncFunctionId |> Option.isNone) then
        raise (NodeRecordTypeException <| sprintf "Sensor with id %A does not have a sync function id" nodeRecord.NodeId)
      let syncFunction = syncFunctions |> Map.find nodeRecord.SyncFunctionId.Value
      createSensor nodeId syncFunction nodeRecord.SyncFunctionId.Value
      |> createNeuronInstance
    | Actuator ->
      if (nodeRecord.OutputHookId |> Option.isNone) then
        raise (NodeRecordTypeException <| sprintf "Actuator with id %A does not have a Output Hook function id" nodeRecord.NodeId)
      let outputHook = outputHooks |> Map.find nodeRecord.OutputHookId.Value
      createActuator nodeId outputHook nodeRecord.OutputHookId.Value
      |> createNeuronInstance

  let connectNeurons fromNodeId ((nodeType : NeuronType), (_, (fromNode : NeuronInstance))) =
    (fun (nodes : NodeRecordConnections) (liveNeurons : Map<NeuronId,NeuronInstance>) ->
      let findNeuronAndAddToOutboundConnections (fromNodeId : NeuronId) (targetNodeId : NeuronId) (weight : Weight) =
        if not <| (liveNeurons |> Map.containsKey targetNodeId) then
          raise (NeuronInstanceException <| sprintf "Trying to connect and can't find a neuron with id %A" targetNodeId)
        let targetNeuron =
          liveNeurons
          |> Map.find targetNodeId

        //Set connection in live neuron
        (targetNeuron,targetNodeId,weight)
        |> NeuronActions.AddOutboundConnection
        |> fromNode.Post

      nodes |> Map.iter (fun targetNodeId weight -> findNeuronAndAddToOutboundConnections fromNodeId targetNodeId weight  )
      fromNode
    )

  let liveNeurons =
    nodeRecords
    |> Map.map createNeuronFromRecord
    |> Map.map connectNeurons

  nodeRecords
  |> Map.filter(fun key value -> value.NodeType = NodeRecordType.Sensor)
  |> Map.map(fun key _ -> liveNeurons |> Map.find key)
