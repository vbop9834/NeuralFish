module NeuralFish.Exporter

open NeuralFish.Core
open NeuralFish.Types

type NodeRecordType =
  | Neuron
  | Sensor
  | Actuator

type NodeRecordConnections = seq<NeuronId*Weight>

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

let constructNodeRecords (liveNeurons : Map<NeuronId,NeuronInstance>) : NodeRecords =
  let generateNodeRecord _ (liveNeuron : NeuronInstance) : NodeRecord =
    let neuronType,outboundConnections = GetNeuronTypeAndOutboundConnections |> liveNeuron.PostAndReply
    let connectionRecords : NodeRecordConnections =  outboundConnections
    match neuronType with
    | NeuronType.Neuron props ->
      {
        NodeId = props.id
        NodeType = Neuron
        OutboundConnections = connectionRecords
        Bias = Some props.bias
        ActivationFunctionId = Some props.activationFunctionId
        SyncFunctionId = None
        OutputHookId = None
      }
    | NeuronType.Actuator props ->
      {
        NodeId = props.id
        NodeType = Actuator
        OutboundConnections = Seq.empty
        Bias = None
        ActivationFunctionId = None
        SyncFunctionId = None
        OutputHookId = Some props.outputHookId
      }
    | NeuronType.Sensor props ->
      {
        NodeId = props.id
        NodeType = Sensor
        OutboundConnections = connectionRecords
        Bias = None
        ActivationFunctionId = None
        SyncFunctionId = Some props.syncFunctionId
        OutputHookId = None
      }

  liveNeurons
  |> Map.map generateNodeRecord

exception NodeRecordTypeException of string
exception NeuronInstanceException of string

type private NeuronIdGeneratorMsg =
  | GetIntId of AsyncReplyChannel<int>

let constructNeuralNetwork (activationFunctions : Map<ActivationFunctionId,ActivationFunction>)  (syncFunctions : Map<SyncFunctionId,SyncFunction>) (outputHooks : Map<OutputHookId,OutputHookFunction>) (nodeRecords : NodeRecords) : Map<NeuronId,NeuronInstance> =
  let createNeuronFromRecord nodeId (nodeRecord : NodeRecord) =
    let (neuronId, neuronInstance) =
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
    neuronInstance

  let connectNeurons (liveNeurons : Map<NeuronId,NeuronInstance>) =
    let connectNode fromNodeId (fromNode : NeuronInstance) =
      let processRecordConnections node =
        let findNeuronAndAddToOutboundConnections (fromNodeId : NeuronId) (targetNodeId : NeuronId) (weight : Weight) =
          if not <| (liveNeurons |> Map.containsKey targetNodeId) then
            raise (NeuronInstanceException <| sprintf "Trying to connect and can't find a neuron with id %A" targetNodeId)
          let targetNeuron =
            liveNeurons
            |> Map.find targetNodeId

          //Set connection in live neuron
          (fun r -> ((targetNeuron,targetNodeId,weight),r) |> NeuronActions.AddOutboundConnection)
          |> fromNode.PostAndReply

        node.OutboundConnections
        |> Seq.iter (fun (targetNodeId,weight) -> findNeuronAndAddToOutboundConnections fromNodeId targetNodeId weight  )
      nodeRecords
      |> Map.find fromNodeId
      |> processRecordConnections

    liveNeurons |> Map.iter connectNode
    liveNeurons

  nodeRecords
  |> Map.map createNeuronFromRecord
  |> connectNeurons
