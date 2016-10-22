module NeuralFish.Exporter

open NeuralFish.Core
open NeuralFish.Types

let constructNodeRecords (liveNeurons : NeuralNetwork) : NodeRecords =
  let generateNodeRecord _ (_,(liveNeuron : NeuronInstance)) : Async<NodeRecord> =
     GetNodeRecord |> liveNeuron.PostAndAsyncReply
  liveNeurons
  |> Map.map generateNodeRecord
  |> Map.map (fun _ asyncthingy -> asyncthingy |> Async.RunSynchronously)

type private NeuronIdGeneratorMsg =
  | GetIntId of AsyncReplyChannel<int>

let constructNeuralNetwork (activationFunctions : Map<ActivationFunctionId,ActivationFunction>)  (syncFunctions : Map<SyncFunctionId,SyncFunction>) (outputHooks : Map<OutputHookId,OutputHookFunction>) (nodeRecords : NodeRecords) : NeuralNetwork =
  let createNeuronFromRecord nodeId (nodeRecord : NodeRecord) =
    let (neuronId, neuronInstance) =
      match nodeRecord.NodeType with
      | NodeRecordType.Neuron ->
        if (nodeRecord.ActivationFunctionId |> Option.isNone) then
          raise (NodeRecordTypeException <| sprintf "Neuron with id %A does not have a Activation function id" nodeRecord.NodeId)
        let activationFunction = activationFunctions |> Map.find nodeRecord.ActivationFunctionId.Value
        let bias =
          match nodeRecord.Bias with
          | Some bias -> bias
          | None -> 0.0
        createNeuron nodeId nodeRecord.Layer activationFunction nodeRecord.ActivationFunctionId.Value bias
        |> createNeuronInstance
      | NodeRecordType.Sensor ->
        if (nodeRecord.SyncFunctionId |> Option.isNone) then
          raise (NodeRecordTypeException <| sprintf "Sensor with id %A does not have a sync function id" nodeRecord.NodeId)
        let syncFunction = syncFunctions |> Map.find nodeRecord.SyncFunctionId.Value
        createSensor nodeId syncFunction nodeRecord.SyncFunctionId.Value
        |> createNeuronInstance
      | NodeRecordType.Actuator ->
        if (nodeRecord.OutputHookId |> Option.isNone) then
          raise (NodeRecordTypeException <| sprintf "Actuator with id %A does not have a Output Hook function id" nodeRecord.NodeId)
        let outputHook = outputHooks |> Map.find nodeRecord.OutputHookId.Value
        createActuator nodeId nodeRecord.Layer outputHook nodeRecord.OutputHookId.Value
        |> createNeuronInstance
    neuronInstance

  let connectNeurons (liveNeurons : Map<NeuronId,NeuronLayerId*NeuronInstance>) =
    let connectNode fromNodeId (_,(fromNode : NeuronInstance)) =
      let processRecordConnections node =
        let findNeuronAndAddToOutboundConnections (fromNodeId : NeuronId) (targetNodeId : NeuronId) (weight : Weight) =
          if not <| (liveNeurons |> Map.containsKey targetNodeId) then
            raise (NeuronInstanceException <| sprintf "Trying to connect and can't find a neuron with id %A" targetNodeId)
          let targetLayer, targetNeuron =
            liveNeurons
            |> Map.find targetNodeId

          //Set connection in live neuron
          (fun r -> ((targetNeuron,targetNodeId,targetLayer,weight),r) |> NeuronActions.AddOutboundConnection)
          |> fromNode.PostAndReply

        node.OutboundConnections
        |> Map.iter (fun _ (targetNodeId, weight) -> findNeuronAndAddToOutboundConnections fromNodeId targetNodeId weight  )
      nodeRecords
      |> Map.find fromNodeId
      |> processRecordConnections

    liveNeurons |> Map.iter connectNode
    liveNeurons

  nodeRecords
  |> Map.map createNeuronFromRecord
  |> connectNeurons
