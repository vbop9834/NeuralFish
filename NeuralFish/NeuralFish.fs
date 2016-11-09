module NeuralFish.Core

open NeuralFish.Types

let mutable InfoLogging = false
let infoLog (message : string) =
  if (InfoLogging) then
    System.Console.WriteLine(message)

let killNeuralNetwork (liveNeurons : NeuralNetwork) =
  let rec waitOnNeuralNetwork neuralNetworkToWaitOn : NeuralNetwork =
    let checkIfNeuralNetworkIsActive (neuralNetwork : NeuralNetwork) =
      //returns true if active
      neuralNetwork
      |> Map.forall(fun i (_,neuron) -> neuron.CurrentQueueLength <> 0)
    if neuralNetworkToWaitOn |> checkIfNeuralNetworkIsActive then
      //200 milliseconds of sleep seems plenty while waiting on the NN
      System.Threading.Thread.Sleep(200)
      waitOnNeuralNetwork neuralNetworkToWaitOn
    else
      neuralNetworkToWaitOn
  let killNeuralNetwork (neuralNetworkToKill : NeuralNetwork) =
    neuralNetworkToKill
    |> Map.toArray
    |> Array.Parallel.iter(fun (_,(_,neuron)) -> Die |> neuron.PostAndReply)

  liveNeurons
  |> waitOnNeuralNetwork
  |> killNeuralNetwork

let activateActuators (neuralNetwork : NeuralNetwork) =
  let activateActuator _ (_,liveNeuron : NeuronInstance) =
    ActivateActuator |>  liveNeuron.Post
  neuralNetwork |> Map.iter activateActuator

let synchronize (_, (_,sensor : NeuronInstance)) =
  Sync |> sensor.Post
  
let synchronizeNN (neuralNetwork : NeuralNetwork) =
  let synchronizeMap _ (_,instance) =
    (None, (None, instance)) |> synchronize
  neuralNetwork
  |> Map.iter synchronizeMap

let synapseDotProduct synapses =
  let rec loop synapses =
    match synapses with
    | [] -> 0.0
    | (_,value,weight)::tail -> value*weight + (loop tail)
  synapses |> Map.toList |> List.map snd |> loop

let createNeuron id layer activationFunction activationFunctionId bias =
  let record =
    {
      NodeId = id
      Layer = layer
      NodeType = NodeRecordType.Neuron
      OutboundConnections = Map.empty
      Bias = Some bias
      ActivationFunctionId = Some activationFunctionId
      SyncFunctionId = None
      OutputHookId = None
      MaximumVectorLength = None
    }
  {
    Record = record
    ActivationFunction = activationFunction
  } |> Neuron
let createSensor id syncFunction syncFunctionId maximumVectorLength =
  let record =
    {
      NodeId = id
      Layer = 0.0
      NodeType = NodeRecordType.Sensor
      OutboundConnections = Map.empty
      Bias = None
      ActivationFunctionId = None
      SyncFunctionId = Some syncFunctionId
      OutputHookId = None
      MaximumVectorLength = Some maximumVectorLength
    }
  {
    Record = record
    SyncFunction = syncFunction
  } |> Sensor
let createActuator id layer outputHook outputHookId =
  let record =
    {
      NodeId = id
      Layer = layer
      NodeType = NodeRecordType.Actuator
      OutboundConnections = Map.empty
      Bias = None
      ActivationFunctionId = None
      SyncFunctionId = None
      OutputHookId = Some outputHookId
      MaximumVectorLength = None
    }
  {
    Record = record
    OutputHook = outputHook
  } |> Actuator

let connectNodeToNeuron (toNodeId, (toNodeLayer, toNode)) weight (fromNodeId, (_,fromNode : NeuronInstance)) =
  (fun r -> ((toNode,toNodeId,toNodeLayer,weight),r) |> NeuronActions.AddOutboundConnection)
  |> fromNode.PostAndReply

let connectNodeToActuator actuator fromNode  =
    connectNodeToNeuron actuator 0.0 fromNode

let connectSensorToNode toNode weights sensor =
 let createConnectionsFromWeight toNode fromNode weight =
   sensor |> connectNodeToNeuron toNode weight
 weights |> Seq.iter (sensor |> createConnectionsFromWeight toNode )

let createNeuronInstance neuronType =
  let nodeId, nodeLayer =
    match neuronType with
      | Neuron props ->
        props.Record.NodeId, props.Record.Layer
      | Sensor props ->
        props.Record.NodeId, props.Record.Layer
      | Actuator props ->
        props.Record.NodeId, props.Record.Layer
  let isBarrierSatisifed (inboundNeuronConnections : InboundNeuronConnections) (barrier : IncomingSynapses) =
    inboundNeuronConnections
    |> Seq.forall(fun connectionId -> barrier |> Map.containsKey connectionId)
  let sendSynapseToNeurons (outputNeurons : NeuronConnections) outputValue =
    let sendSynapseToNeuron outputValue neuronConnectionId outputNeuronConnection =
      (neuronConnectionId, (outputNeuronConnection.NodeId, outputValue, outputNeuronConnection.Weight), true)
      |> ReceiveInput
      |> outputNeuronConnection.Neuron.Post
    outputNeurons
    |> Map.iter (sendSynapseToNeuron outputValue)
  let addBias bias outputVal =
    outputVal + bias
  let activateNeuron (barrier : IncomingSynapses) (outboundConnections : NeuronConnections) neuronType =
    match neuronType with
    | Neuron props ->
      let logNeuronOutput nodeId activationFunctionId bias outputValue =
        sprintf "Neuron %A is outputting %A after activation %A and bias %A" nodeId outputValue activationFunctionId bias
        |> infoLog
        outputValue

      let someBias =
        match props.Record.Bias with
        | Some bias -> bias
        | None -> 0.0
      barrier
      |> synapseDotProduct
      |> addBias someBias
      |> props.ActivationFunction
      |> logNeuronOutput props.Record.NodeId props.Record.ActivationFunctionId props.Record.Bias
      |> sendSynapseToNeurons outboundConnections
    | Actuator props ->
      let logActuatorOutput nodeId outputHookId outputValue =
        sprintf "Actuator %A is outputting %A with output hook %A" nodeId outputValue outputHookId
        |> infoLog
        outputValue
      barrier
      |> Map.toSeq
      |> Seq.sumBy (fun (_,(_,value,weight)) -> value)
      |> logActuatorOutput props.Record.NodeId props.Record.OutputHookId
      |> props.OutputHook
    | Sensor _ ->
      ()

  let createInactiveNeuronConnection activeNeuronConnection =
    activeNeuronConnection.NodeId, activeNeuronConnection.Weight

  let neuronInstance = NeuronInstance.Start(fun inbox ->
    let rec loop (barrier : AxonHillockBarrier)
                   (inboundConnections : InboundNeuronConnections)
                     (outboundConnections : NeuronConnections)
                       maximumVectorLength
                         (maybeCortex : bool option) =
      async {
        let! someMsg = inbox.TryReceive 250
        match someMsg with
        | None ->
          // sprintf "Neuron %A did not receive message in 250 ms. Looping mailbox" nodeId |> infoLog
          return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex
        | Some msg ->
          match msg with
          | Sync ->
            match neuronType with
            | Neuron _ ->
              return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex
            | Actuator _ ->
              return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex
            | Sensor props ->
              let inflateData expectedVectorLength (dataStream : NeuronOutput seq) =
                let inflatedData =
                  let difference = expectedVectorLength - (dataStream |> Seq.length)
                  [0..difference]
                  |> Seq.map (fun _ -> 0.0)
                Seq.append dataStream inflatedData
              let outboundConnectionsSeq = outboundConnections |> Map.toSeq
              let unInflatedDataStream = props.SyncFunction()
              let newMaximumVectorLength =
                let actualDataVectorLength = unInflatedDataStream |> Seq.length
                if (actualDataVectorLength > maximumVectorLength) then
                  actualDataVectorLength 
                else
                  maximumVectorLength
              let dataStream =
                unInflatedDataStream
                |> inflateData (outboundConnectionsSeq |> Seq.length)
              let rec processSensorSync dataStream remainingConnections =
                if (dataStream |> Seq.isEmpty || remainingConnections |> Seq.isEmpty) then
                  ()
                else
                  let sendSynapseToNeuron (neuron : NeuronInstance) neuronConnectionId weight outputValue =
                    (neuronConnectionId, (props.Record.NodeId, outputValue, weight), true)
                    |> ReceiveInput
                    |> neuron.Post
                  let data = dataStream |> Seq.head
                  let (connectionId, connection) = remainingConnections |> Seq.head 
                  sprintf "Sending %A to connection %A with a weight of %A" data connectionId connection.Weight |> infoLog
                  data |> sendSynapseToNeuron connection.Neuron connectionId connection.Weight
                  let newDataStream = (dataStream |> Seq.tail)
                  processSensorSync newDataStream (remainingConnections |> Seq.tail)
              processSensorSync dataStream outboundConnectionsSeq
              return! loop barrier inboundConnections outboundConnections newMaximumVectorLength maybeCortex
          | ReceiveInput (neuronConnectionId, package, activateIfBarrierIsFull) ->
            let updatedBarrier : IncomingSynapses =
              barrier
              |> Map.add neuronConnectionId package
            match neuronType with
            | Neuron props ->
              if (activateIfBarrierIsFull && updatedBarrier |> isBarrierSatisifed inboundConnections) then
                sprintf "Barrier is satisfied for Neuron %A. Received %A from %A" props.Record.NodeId package neuronConnectionId |> infoLog
                neuronType |> activateNeuron updatedBarrier outboundConnections
                return! loop Map.empty inboundConnections outboundConnections maximumVectorLength maybeCortex
              else
                sprintf "Barrier not satisfied for Neuron %A. Received %A from %A" props.Record.NodeId package neuronConnectionId |> infoLog
                return! loop updatedBarrier inboundConnections outboundConnections maximumVectorLength maybeCortex
            | Actuator props ->
              if (activateIfBarrierIsFull && updatedBarrier |> isBarrierSatisifed inboundConnections) then
                match maybeCortex with
                | None ->
                  sprintf "Barrier is satisifed for Actuator %A" props.Record.NodeId |> infoLog
                  neuronType |> activateNeuron updatedBarrier outboundConnections
                  return! loop Map.empty inboundConnections outboundConnections maximumVectorLength maybeCortex
                | Some _ ->
                  sprintf "Barrier is satisifed for Actuator %A. Not activating due to registered cortex. Waiting for a signal from the cortex" props.Record.NodeId |> infoLog
                  let readyToActivate = Some true
                  return! loop updatedBarrier inboundConnections outboundConnections maximumVectorLength readyToActivate
              else
                sprintf "Node %A not activated. Received %A from %A" props.Record.NodeId package neuronConnectionId |> infoLog
                return! loop updatedBarrier inboundConnections outboundConnections maximumVectorLength maybeCortex
            | Sensor _ ->
              //Sensors use the sync msg
              return! loop Map.empty inboundConnections outboundConnections maximumVectorLength maybeCortex
          | AddOutboundConnection ((toNode,nodeId,outboundLayer,weight),replyChannel) ->
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

              //queue up blank synapses for recurrent connections
              if (nodeLayer >= outboundLayer) then
                sprintf "Sending recurrent blank synapse to %A via %A" nodeId neuronConnectionId |> infoLog 
                (neuronConnectionId, (nodeId,0.0,weight), false)
                |> ReceiveInput
                |> toNode.Post

              sprintf "Node %A is adding Node %A as an outbound connection %A with weight %A" neuronType nodeId neuronConnectionId weight
              |> infoLog
              return! loop barrier inboundConnections updatedOutboundConnections maximumVectorLength maybeCortex
            | AddInboundConnection (neuronConnectionId,replyChannel) ->
              let updatedInboundConnections =
                inboundConnections |> Seq.append(Seq.singleton neuronConnectionId)
              replyChannel.Reply()
              sprintf "Added inbound neuron connection %A" neuronConnectionId
              |> infoLog
              return! loop barrier updatedInboundConnections outboundConnections maximumVectorLength maybeCortex
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
                  let maximumVectorLengthToRecord =
                    match props.Record.MaximumVectorLength with
                      | Some previousMaximumVectorLength ->
                        if (maximumVectorLength > previousMaximumVectorLength) then
                          maximumVectorLength
                        else
                          previousMaximumVectorLength
                      | None -> maximumVectorLength 
                  { props.Record with OutboundConnections = outboundNodeRecordConnections; MaximumVectorLength = Some maximumVectorLengthToRecord }
                | Actuator props -> props.Record
              nodeRecord |> replyChannel.Reply
              return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex
            | Die replyChannel ->
              replyChannel.Reply()
              ()
            | RegisterCortex (cortex,replyChannel) ->
              match neuronType with
              | Actuator _ ->
                let someReadyToActivate = Some false
                replyChannel.Reply ()
                return! loop barrier inboundConnections outboundConnections maximumVectorLength someReadyToActivate 
              | _ ->
                replyChannel.Reply ()
                return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex 
            | ActivateActuator ->
              match maybeCortex with
              | None ->
                return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex
              | Some readyToActivate ->
                if readyToActivate then
                  match neuronType with
                  | Actuator _ ->
                    sprintf "Activating Actuator %A" nodeId |> infoLog
                    neuronType |> activateNeuron barrier outboundConnections
                    let notReadyToActivate = Some false
                    return! loop Map.empty inboundConnections outboundConnections maximumVectorLength notReadyToActivate
                  | _ ->
                    return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex
                else
                  return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex
            | CheckActuatorStatus replyChannel ->  
              match maybeCortex with
              | None ->
                true |> replyChannel.Reply 
              | Some readyToActivate ->
                readyToActivate |> replyChannel.Reply
              return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex
      }
    loop Map.empty Seq.empty Map.empty 0 None
  )

  (nodeId, (nodeLayer, neuronInstance))
