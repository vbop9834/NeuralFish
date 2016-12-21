module NeuralFish.Core

open NeuralFish.Types
open NeuralFish.Exceptions

let sigmoid = (fun x -> 1.0 / (1.0 + exp(-x)))

let defaultInfoLog (message : string) =
  message |> System.Console.WriteLine

//TODO configurable timeout for this
let killNeuralNetwork (liveNeurons : NeuralNetwork) =
  let rec waitOnNeuralNetwork neuralNetworkToWaitOn : NeuralNetwork =
    let checkIfNeuralNetworkIsActive (neuralNetwork : NeuralNetwork) =
      //returns true if active
      neuralNetwork
      |> Map.exists(fun i (nodeRecordId,neuron) -> neuron.CurrentQueueLength <> 0)
    if neuralNetworkToWaitOn |> checkIfNeuralNetworkIsActive then
      //200 milliseconds of sleep seems plenty while waiting on the NN
      //TODO make this configurable
      System.Threading.Thread.Sleep(200)
      waitOnNeuralNetwork neuralNetworkToWaitOn
    else
      neuralNetworkToWaitOn
  let killNeuralNetwork (neuralNetworkToKill : NeuralNetwork) =
    neuralNetworkToKill
    |> Map.toArray
    |> Array.Parallel.iter(fun (_,(_,neuron)) -> neuron.TryPostAndReply (Die, timeout=500) |> ignore)

  liveNeurons
  |> waitOnNeuralNetwork
  |> killNeuralNetwork

let activateActuators (neuralNetwork : NeuralNetwork) =
  let activateActuator (_,(_,liveNeuron : NeuronInstance)) =
    let didPost = ActivateActuator |> liveNeuron.TryPostAndReply
    match didPost with
    | None ->
      raise <| NeuronInstanceUnavailableException "Core - Neuron unable to activate due to instance being unavailable"
    | Some _ -> ()
  neuralNetwork
  |> Map.toArray
  |> Array.Parallel.iter activateActuator

let synchronize (_, (_,sensor : NeuronInstance)) =
  Sync |> sensor.Post

let synchronizeNN (neuralNetwork : NeuralNetwork) =
  let synchronizeMap _ (_,instance) =
    (None, (None, instance)) |> synchronize
  neuralNetwork
  |> Map.iter synchronizeMap

let synapseDotProduct (weightedSynapses : WeightedSynapses) =
  let rec loop synapses =
    match synapses with
    | [] -> 0.0
    | (value, inboundConnection)::tail -> value*inboundConnection.Weight + (loop tail)
  weightedSynapses |> Map.toList |> List.map snd |> loop


let createNeuronFromRecord (activationFunction : ActivationFunction) (record : NodeRecord) : NeuronType =
  {
    Record = record
    ActivationFunction = activationFunction
  } |> NeuronType.Neuron

let createNeuron id layer activationFunction activationFunctionId bias learningAlgorithm =
   {
     NodeId = id
     Layer = layer
     NodeType = NodeRecordType.Neuron
     InboundConnections = Map.empty
     Bias = Some bias
     ActivationFunctionId = Some activationFunctionId
     SyncFunctionId = None
     OutputHookId = None
     MaximumVectorLength = None
     NeuronLearningAlgorithm = learningAlgorithm
   } |> createNeuronFromRecord activationFunction

let createSensorFromRecord syncFunction record =
  {
    Record = record
    SyncFunction = syncFunction
  } |> Sensor

let createSensor id syncFunction syncFunctionId maximumVectorLength =
  {
    NodeId = id
    Layer = 0.0
    NodeType = NodeRecordType.Sensor
    InboundConnections = Map.empty
    Bias = None
    ActivationFunctionId = None
    SyncFunctionId = Some syncFunctionId
    OutputHookId = None
    MaximumVectorLength = Some maximumVectorLength
    NeuronLearningAlgorithm = NoLearning
  } |> createSensorFromRecord syncFunction

let createActuatorFromRecord outputHook record =
  {
    Record = record
    OutputHook = outputHook
  } |> Actuator

let createActuator id layer outputHook outputHookId =
  {
    NodeId = id
    Layer = layer
    NodeType = NodeRecordType.Actuator
    InboundConnections = Map.empty
    Bias = None
    ActivationFunctionId = None
    SyncFunctionId = None
    OutputHookId = Some outputHookId
    MaximumVectorLength = None
    NeuronLearningAlgorithm = NoLearning
  } |> createActuatorFromRecord outputHook

let connectNodeToNeuron (toNodeId, (toNodeLayer, toNode)) weight (fromNodeId, (_,fromNode : NeuronInstance)) =
  (fun r -> ((toNode,toNodeId,toNodeLayer,weight),r) |> NeuronActions.AddOutboundConnection)
  |> fromNode.PostAndReply

let connectNodeToActuator actuator fromNode  =
    connectNodeToNeuron actuator 0.0 fromNode

let connectSensorToNode toNode weights sensor =
 let createConnectionsFromWeight toNode fromNode weight =
   sensor |> connectNodeToNeuron toNode weight
 weights |> Seq.iter (sensor |> createConnectionsFromWeight toNode )

let createNeuronInstance infoLog neuronType =
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
    |> Map.forall(fun connectionId _ -> barrier |> Map.containsKey connectionId)
  let addBias bias outputVal =
    outputVal + bias
  let activateNeuron (barrier : WeightedSynapses) (outboundConnections : NeuronConnections) (neuronProps : NeuronProperties) =
    let sendSynapseToNeurons (outputNeurons : NeuronConnections) outputValue =
      let sendSynapseToNeuron outputValue neuronConnectionId outputNeuronConnection =
        (neuronConnectionId, outputValue, true)
        |> ReceiveInput
        |> outputNeuronConnection.Neuron.Post
      outputNeurons
      |> Map.iter (sendSynapseToNeuron outputValue)
      outputValue
    let logNeuronOutput nodeId activationFunctionId bias outputValue =
      sprintf "Neuron %A is outputting %A after activation %A and bias %A" nodeId outputValue activationFunctionId bias
      |> infoLog
      outputValue

    let someBias =
      match neuronProps.Record.Bias with
      | Some bias -> bias
      | None -> 0.0
    barrier
    |> synapseDotProduct
    |> addBias someBias
    |> neuronProps.ActivationFunction
    |> logNeuronOutput neuronProps.Record.NodeId neuronProps.Record.ActivationFunctionId neuronProps.Record.Bias
    |> sendSynapseToNeurons outboundConnections
  let activateActuator (barrier : IncomingSynapses) actuatorProps =
      let logActuatorOutput outputHookId outputValue =
        sprintf "Actuator %A is outputting %A with output hook %A" nodeId outputValue outputHookId
        |> infoLog
        outputValue
      barrier
      |> Map.toSeq
      |> Seq.sumBy (fun (_,value) -> value)
      |> logActuatorOutput actuatorProps.Record.OutputHookId
      |> actuatorProps.OutputHook

  let neuronInstance = NeuronInstance.Start(fun inbox ->
    let rec loop (barrier : AxonHillockBarrier)
                   (inboundConnections : InboundNeuronConnections)
                     (outboundConnections : NeuronConnections)
                       (maximumVectorLength : int)
                         (maybeCortex : bool option) =
      async {
        let! someMsg = inbox.TryReceive 250
        match someMsg with
        | None ->
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
              let deflatedDataStream = props.SyncFunction()
              let newMaximumVectorLength =
                let actualDataVectorLength = deflatedDataStream |> Seq.length
                if (actualDataVectorLength > maximumVectorLength) then
                  actualDataVectorLength
                else
                  maximumVectorLength
              let dataStream =
                deflatedDataStream
                |> inflateData (outboundConnectionsSeq |> Seq.length)
              let rec processSensorSync dataStream remainingConnections =
                if (dataStream |> Seq.isEmpty || remainingConnections |> Seq.isEmpty) then
                  ()
                else
                  let sendSynapseToNeuron (neuron : NeuronInstance) neuronConnectionId outputValue =
                    (neuronConnectionId, outputValue, true)
                    |> ReceiveInput
                    |> neuron.Post
                  let data = dataStream |> Seq.head
                  let (connectionId, connection) = remainingConnections |> Seq.head
                  sprintf "Sending %A to connection %A" data connectionId |> infoLog
                  data |> sendSynapseToNeuron connection.Neuron connectionId
                  let newDataStream = (dataStream |> Seq.tail)
                  let updatedRemainingConnections = (remainingConnections |> Seq.tail)
                  processSensorSync newDataStream updatedRemainingConnections
              if outboundConnectionsSeq |> Seq.isEmpty then
                let exceptionMsg = sprintf "Sensor %A does not have any outbound connections" nodeId
                raise <| SensorInstanceDoesNotHaveAnyOutboundConnections(exceptionMsg)
              else
                processSensorSync dataStream outboundConnectionsSeq
              return! loop barrier inboundConnections outboundConnections newMaximumVectorLength maybeCortex
          | ReceiveInput (neuronConnectionId, package, activateIfBarrierIsFull) ->
            let processLearning learningAlgorithm (weightedSynapses : WeightedSynapses) (neuronOutput : NeuronOutput) : InboundNeuronConnections =
              let processLearningForConnection neuronConnectionId (inputValue, inboundConnection) =
                match learningAlgorithm with
                | NoLearning -> inboundConnection
                | Hebbian learningRateCoefficient ->
                  let newWeight = inboundConnection.Weight + learningRateCoefficient*inputValue*neuronOutput
                  { inboundConnection with
                      Weight = newWeight
                  }
              weightedSynapses
              |> Map.map processLearningForConnection

            let updatedBarrier : IncomingSynapses =
              barrier
              |> Map.add neuronConnectionId package
            match neuronType with
            | Neuron props ->
              if (activateIfBarrierIsFull && updatedBarrier |> isBarrierSatisifed inboundConnections) then
                sprintf "Barrier is satisfied for Neuron %A. Received %A from %A" props.Record.NodeId package neuronConnectionId |> infoLog
                let weightedSynapses : WeightedSynapses =
                  let getWeightedSynapse neuronConnectionId (synapse : Synapse)  : WeightedSynapse =
                    let inboundConnection =
                      match inboundConnections |> Map.tryFind neuronConnectionId with
                      | None -> raise <| MissingInboundConnectionException(sprintf "Neuron %A is missing inbound connection %A" nodeId neuronConnectionId)
                      | Some x -> x
                    synapse, inboundConnection
                  updatedBarrier
                  |> Map.map getWeightedSynapse
                let neuronOutput = props |> activateNeuron weightedSynapses outboundConnections
                let updatedInboundConnections =
                  processLearning props.Record.NeuronLearningAlgorithm weightedSynapses neuronOutput
                return! loop Map.empty updatedInboundConnections outboundConnections maximumVectorLength maybeCortex
              else
                sprintf "Barrier not satisfied for Neuron %A. Received %A from %A" props.Record.NodeId package neuronConnectionId |> infoLog
                return! loop updatedBarrier inboundConnections outboundConnections maximumVectorLength maybeCortex
            | Actuator props ->
              if (activateIfBarrierIsFull && updatedBarrier |> isBarrierSatisifed inboundConnections) then
                match maybeCortex with
                | None ->
                  sprintf "Barrier is satisifed for Actuator %A" props.Record.NodeId |> infoLog
                  props |> activateActuator updatedBarrier
                  return! loop Map.empty inboundConnections outboundConnections maximumVectorLength maybeCortex
                | Some _ ->
                  sprintf "Barrier is satisifed for Actuator %A. Not activating due to registered cortex. Waiting for a signal from the cortex" props.Record.NodeId |> infoLog
                  let readyToActivate = Some true
                  return! loop updatedBarrier inboundConnections outboundConnections maximumVectorLength readyToActivate
              else
                sprintf "Node %A not activated. Received %A from %A" nodeId package neuronConnectionId |> infoLog
                return! loop updatedBarrier inboundConnections outboundConnections maximumVectorLength maybeCortex
            | Sensor _ ->
              //Sensors use the sync msg
              return! loop Map.empty inboundConnections outboundConnections maximumVectorLength maybeCortex
          | NeuronActions.AddOutboundConnection ((toNode,toNodeId,outboundLayer,weight),replyChannel) ->
              let neuronConnectionId = System.Guid.NewGuid()
              let updatedOutboundConnections =
                let outboundConnection =
                 {
                  InitialWeight = weight
                  NodeId = toNodeId
                  Neuron = toNode
                 }
                outboundConnections |> Map.add neuronConnectionId outboundConnection

              (neuronConnectionId, nodeId, weight, replyChannel)
              |> NeuronActions.AddInboundConnection
              |> toNode.Post

              //queue up blank synapses for recurrent connections
              let tolerance = System.BitConverter.DoubleToInt64Bits(0.001)
              if abs(System.BitConverter.DoubleToInt64Bits(nodeLayer) - System.BitConverter.DoubleToInt64Bits(outboundLayer)) <= tolerance then
                sprintf "Node %A Sending recurrent blank synapse to %A via %A" nodeId toNodeId neuronConnectionId |> infoLog
                (neuronConnectionId, 0.0, false)
                |> ReceiveInput
                |> toNode.Post

              sprintf "Node %A is adding Node %A as an outbound connection %A with weight %A" neuronType toNodeId neuronConnectionId weight
              |> infoLog
              return! loop barrier inboundConnections updatedOutboundConnections maximumVectorLength maybeCortex
            | NeuronActions.AddInboundConnection (neuronConnectionId, fromNodeId, weight, replyChannel) ->
              let updatedInboundConnections =
                let newInboundConnection =
                  {
                    Weight = weight
                    InitialWeight = weight
                    FromNodeId = fromNodeId
                  }
                inboundConnections
                |> Map.add neuronConnectionId newInboundConnection
              replyChannel.Reply()
              sprintf "Added inbound neuron connection %A" neuronConnectionId
              |> infoLog
              return! loop barrier updatedInboundConnections outboundConnections maximumVectorLength maybeCortex
            | GetNodeRecord replyChannel ->
              async {
                let inactiveConnections : NodeRecordConnections =
                  let createInactiveConnection _ (inboundNeuronConnection : InboundNeuronConnection) : InactiveNeuronConnection =
                    inboundNeuronConnection.FromNodeId, inboundNeuronConnection.InitialWeight
                  inboundConnections
                  |> Map.map createInactiveConnection
                let nodeRecord =
                  match neuronType with
                  | Neuron props -> props.Record
                  | Sensor props ->
                    let maximumVectorLengthToRecord =
                      match props.Record.MaximumVectorLength with
                        | Some previousMaximumVectorLength ->
                          if (maximumVectorLength > previousMaximumVectorLength) then
                            maximumVectorLength
                          else
                            previousMaximumVectorLength
                        | None -> maximumVectorLength
                    { props.Record with MaximumVectorLength = Some maximumVectorLengthToRecord }
                  | Actuator props -> props.Record
                let nodeRecordWithConnections = { nodeRecord with InboundConnections = inactiveConnections}
                nodeRecordWithConnections |> replyChannel.Reply
              } |> Async.Start |> ignore
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
            | ActivateActuator replyChannel ->
              match maybeCortex with
              | None ->
                replyChannel.Reply()
                return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex
              | Some readyToActivate ->
                if readyToActivate then
                  match neuronType with
                  | Actuator props ->
                    sprintf "Activating Actuator %A" nodeId |> infoLog
                    props |> activateActuator barrier
                    replyChannel.Reply ()
                    let notReadyToActivate = Some false
                    return! loop Map.empty inboundConnections outboundConnections maximumVectorLength notReadyToActivate
                  | _ ->
                    replyChannel.Reply ()
                    return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex
                else
                  replyChannel.Reply ()
                  return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex
            | CheckActuatorStatus replyChannel ->
              match maybeCortex with
              | None ->
                true |> replyChannel.Reply
              | Some readyToActivate ->
                readyToActivate |> replyChannel.Reply
              return! loop barrier inboundConnections outboundConnections maximumVectorLength maybeCortex
      }
    loop Map.empty Map.empty Map.empty 0 None
  )

  //Add exception logging
  neuronInstance |> (fun x -> x.Error.Add(fun err -> sprintf "%A" err |> infoLog))

  (nodeId, (nodeLayer, neuronInstance))
