module NeuralFish.Core

open NeuralFish.Types

let synchronize (_, (sensor : NeuronInstance)) =
  Sync |> sensor.Post

let synapseDotProduct synapses =
  let rec loop synapses =
    match synapses with
    | [] -> 0.0
    | (_,value,weight)::tail -> value*weight + (loop tail)
  synapses |> Map.toList |> List.map snd |> loop

let createNeuron id activationFunction activationFunctionId bias =
  let record =
    {
      NodeId = id
      NodeType = NodeRecordType.Neuron
      OutboundConnections = Map.empty
      Bias = Some bias
      ActivationFunctionId = Some activationFunctionId
      SyncFunctionId = None
      OutputHookId = None
    }
  {
    Record = record
    ActivationFunction = activationFunction
  } |> Neuron
let createSensor id syncFunction syncFunctionId =
  let record =
    {
      NodeId = id
      NodeType = NodeRecordType.Sensor
      OutboundConnections = Map.empty
      Bias = None
      ActivationFunctionId = None
      SyncFunctionId = Some syncFunctionId
      OutputHookId = None
    }
  {
    Record = record
    SyncFunction = syncFunction
  } |> Sensor
let createActuator id outputHook outputHookId =
  let record =
    {
      NodeId = id
      NodeType = NodeRecordType.Actuator
      OutboundConnections = Map.empty
      Bias = None
      ActivationFunctionId = None
      SyncFunctionId = None
      OutputHookId = Some outputHookId
    }
  {
    Record = record
    OutputHook = outputHook
  } |> Actuator

let connectNodeToNeuron (toNodeId, toNode) weight (fromNodeId, (fromNode : NeuronInstance)) =
  (fun r -> ((toNode,toNodeId,weight),r) |> NeuronActions.AddOutboundConnection)
  |> fromNode.PostAndReply

let connectNodeToActuator actuator fromNode  =
    connectNodeToNeuron actuator 0.0 fromNode

let connectSensorToNode toNode weights sensor =
 let createConnectionsFromWeight toNode fromNode weight =
   sensor |> connectNodeToNeuron toNode weight
 weights |> Seq.iter (sensor |> createConnectionsFromWeight toNode )

let createNeuronInstance neuronType =
  let getNodeIdFromProps neuronType =
    match neuronType with
      | Neuron props ->
        props.Record.NodeId
      | Sensor props ->
        props.Record.NodeId
      | Actuator props ->
        props.Record.NodeId
  let isBarrierSatisifed (inboundNeuronConnections : InboundNeuronConnections) (barrier : IncomingSynapses) =
    inboundNeuronConnections
    |> Seq.forall(fun connectionId -> barrier |> Map.containsKey connectionId)
  let sendSynapseToNeurons (outputNeurons : NeuronConnections) outputValue =
    let sendSynapseToNeuron outputValue neuronConnectionId outputNeuronConnection =
      (neuronConnectionId, (outputNeuronConnection.NodeId, outputValue, outputNeuronConnection.Weight))
      |> ReceiveInput
      |> outputNeuronConnection.Neuron.Post
    outputNeurons
    |> Map.iter (sendSynapseToNeuron outputValue)
  let addBias bias outputVal =
    outputVal + bias
  let activateNeuron (barrier : IncomingSynapses) (outboundConnections : NeuronConnections) neuronType =
    match neuronType with
    | Neuron props ->
      let someBias =
        match props.Record.Bias with
        | Some bias -> bias
        | None ->
          raise (NoBiasInRecordForNeuronException <| sprintf "Neuron %A does not have a bias" props.Record.NodeId)
      barrier
      |> synapseDotProduct
      |> addBias someBias
      |> props.ActivationFunction
      |> sendSynapseToNeurons outboundConnections
    | Actuator props ->
      barrier
      |> Map.map(fun connectionId (neuronId,value,weight) -> value)
      |> Map.toSeq |> Seq.map snd |> Seq.sum
      |> props.OutputHook
    | Sensor _ ->
      ()

  let createInactiveNeuronConnection activeNeuronConnection =
    activeNeuronConnection.NodeId, activeNeuronConnection.Weight

  let neuronInstance = NeuronInstance.Start(fun inbox ->
    let rec loop barrier (inboundConnections : InboundNeuronConnections) (outboundConnections : NeuronConnections) =
      async {
        let! someMsg = inbox.TryReceive 20000
        match someMsg with
        | None ->
          return! loop barrier inboundConnections outboundConnections
        | Some msg ->
          match msg with
          | Sync ->
            match neuronType with
            | Neuron _ ->
              return! loop barrier inboundConnections outboundConnections
            | Actuator _ ->
              return! loop barrier inboundConnections outboundConnections
            | Sensor props ->
              let rec processSensorSync dataStream connectionId connection =
                let sendSynapseToNeuron (neuron : NeuronInstance) neuronConnectionId weight outputValue =
                  (neuronConnectionId, (props.Record.NodeId, outputValue, weight))
                  |> ReceiveInput
                  |> neuron.Post

                let data = dataStream |> Seq.head
                data |> sendSynapseToNeuron connection.Neuron connectionId connection.Weight
              outboundConnections
              |> Map.iter (processSensorSync <| props.SyncFunction())
              return! loop barrier inboundConnections outboundConnections
          | ReceiveInput (neuronConnectionId, package) ->
            let updatedBarrier : IncomingSynapses =
              barrier
              |> Map.add neuronConnectionId package
            match neuronType with
            | Neuron props ->
              if updatedBarrier |> isBarrierSatisifed inboundConnections then
                neuronType |> activateNeuron updatedBarrier outboundConnections
                return! loop Map.empty inboundConnections outboundConnections
              else
                return! loop updatedBarrier inboundConnections outboundConnections
            | Actuator props ->
              if updatedBarrier |> isBarrierSatisifed inboundConnections then
                neuronType |> activateNeuron updatedBarrier outboundConnections
                return! loop Map.empty inboundConnections outboundConnections
              else
                return! loop updatedBarrier inboundConnections outboundConnections
            | Sensor _ ->
              //Sensors use the sync msg
              return! loop Map.empty inboundConnections outboundConnections
          | AddOutboundConnection ((toNode,nodeId,weight),replyChannel) ->
              let neuronConnectionId = System.Guid.NewGuid()
              let updatedOutboundConnections =
                let outboundConnection =
                 {
                  NodeId = nodeId
                  Neuron = toNode
                  Weight = weight
                 }
                outboundConnections |> Map.add neuronConnectionId outboundConnection

              (neuronConnectionId, replyChannel)
              |> AddInboundConnection
              |> toNode.Post

              return! loop barrier inboundConnections updatedOutboundConnections
            | AddInboundConnection (neuronConnectionId,replyChannel) ->
              let updatedInboundConnections =
                inboundConnections |> Seq.append(Seq.singleton neuronConnectionId)
              replyChannel.Reply()
              return! loop barrier updatedInboundConnections outboundConnections
            | GetNodeRecord replyChannel ->
              let getOutboundNodeRecordConnections () : NodeRecordConnections =
                outboundConnections
                |> Map.map (fun neuronConnectionId neuronConnection -> neuronConnection |> createInactiveNeuronConnection)
              let nodeRecord =
                match neuronType with
                | Neuron props ->
                  let outboundNodeRecordConnections = getOutboundNodeRecordConnections ()
                  { props.Record with OutboundConnections = outboundNodeRecordConnections }
                | Sensor props ->
                  let outboundNodeRecordConnections = getOutboundNodeRecordConnections ()
                  { props.Record with OutboundConnections = outboundNodeRecordConnections }
                | Actuator props -> props.Record
              nodeRecord |> replyChannel.Reply
              return! loop barrier inboundConnections outboundConnections

      }
    loop Map.empty Seq.empty Map.empty
  )

  (neuronType |> getNodeIdFromProps, neuronInstance)
