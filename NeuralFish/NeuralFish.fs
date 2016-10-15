module NeuralFish.Core

open NeuralFish.Types

let synchronize (_, (sensor : NeuronInstance)) =
  Sync |> sensor.Post

let synapseDotProduct synapses =
  let rec loop synapses =
    match synapses with
    | [] -> 0.0
    | (_,value,weight)::tail -> value*weight + (loop tail)
  printfn "computed %A" synapses
  synapses |> Map.toList |> List.map snd |> loop

let createNeuron id activationFunction activationFunctionId bias =
  {
    id = id
    bias = bias
    activationFunction = activationFunction
    activationFunctionId = activationFunctionId
  } |> Neuron
let createSensor id syncFunction syncFunctionId =
  {
    id = id
    syncFunction = syncFunction
    syncFunctionId = syncFunctionId
  } |> Sensor
let createActuator id outputHook outputHookId =
  {
    id = id
    outputHook = outputHook
    outputHookId = outputHookId
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
        props.id
      | Sensor props ->
        props.id
      | Actuator props ->
        props.id
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
    printfn "activated %A" neuronType
    match neuronType with
    | Neuron props ->
      barrier
      |> synapseDotProduct
      |> addBias props.bias
      |> props.activationFunction
      |> sendSynapseToNeurons outboundConnections
    | Actuator props ->
      barrier
      |> Map.map(fun connectionId (neuronId,value,weight) -> value)
      |> Map.toSeq |> Seq.map snd |> Seq.sum
      |> props.outputHook
    | Sensor _ ->
      ()

  let createInactiveNeuronConnection activeNeuronConnection =
    activeNeuronConnection.NodeId, activeNeuronConnection.Weight

  let neuronInstance = NeuronInstance.Start(fun inbox ->
    let rec loop barrier (inboundConnections : InboundNeuronConnections) (outboundConnections : NeuronConnections) =
      async {
        let! msg = inbox.Receive ()
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
                (neuronConnectionId, (props.id, outputValue, weight))
                |> ReceiveInput
                |> neuron.Post

              let data = dataStream |> Seq.head
              data |> sendSynapseToNeuron connection.Neuron connectionId connection.Weight
            outboundConnections
            |> Map.iter (processSensorSync <| props.syncFunction())
            return! loop barrier inboundConnections outboundConnections
        | ReceiveInput (neuronConnectionId, package) ->
          let updatedBarrier : IncomingSynapses =
            barrier
            |> Map.add neuronConnectionId package
          printfn "Inbound is: %A" <| List.ofSeq inboundConnections
          printfn "Barrier is: %A" updatedBarrier
          match neuronType with
          | Neuron props ->
            if updatedBarrier |> isBarrierSatisifed inboundConnections then
              printfn "Barrier activated"
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

          (neuronConnectionId, replyChannel) |> AddInboundConnection |> toNode.Post

          return! loop barrier inboundConnections updatedOutboundConnections
        | AddInboundConnection (neuronConnectionId,replyChannel) ->
          let updatedInboundConnections =
            inboundConnections |> Seq.append(Seq.singleton neuronConnectionId)
          replyChannel.Reply()
          return! loop barrier updatedInboundConnections outboundConnections
        | GetNeuronTypeAndOutboundConnections replyChannel ->
          outboundConnections
          |> Map.map (fun key neuronConnection -> neuronConnection |> createInactiveNeuronConnection)
          |> Map.toSeq |> Seq.map snd
          |> (fun outboundConnections -> neuronType, outboundConnections)
          |> replyChannel.Reply
          return! loop barrier inboundConnections outboundConnections
      }
    loop Map.empty Seq.empty Map.empty
  )

  (neuronType |> getNodeIdFromProps, neuronInstance)
