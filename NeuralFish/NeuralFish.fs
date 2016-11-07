module NeuralFish.Core

open NeuralFish.Types

let mutable InfoLogging = true
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
    }
  {
    Record = record
    ActivationFunction = activationFunction
  } |> Neuron
let createSensor id syncFunction syncFunctionId =
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
                     (outboundConnections : NeuronConnections) =
      async {
        let! someMsg = inbox.TryReceive 250
        match someMsg with
        | None ->
          // sprintf "Neuron %A did not receive message in 250 ms. Looping mailbox" nodeId |> infoLog
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
              let dataStream = props.SyncFunction()
              let outboundConnectionsSeq = outboundConnections |> Map.toSeq
              let rec processSensorSync dataStream remainingConnections =
                if (dataStream |> Seq.isEmpty || remainingConnections |> Seq.isEmpty) then
                  ()
                else
                  let sendSynapseToNeuron (neuron : NeuronInstance) neuronConnectionId weight outputValue =
                    (neuronConnectionId, (props.Record.NodeId, outputValue, weight))
                    |> ReceiveInput
                    |> neuron.Post

                  let data = dataStream |> Seq.head
                  let (connectionId, connection) = remainingConnections |> Seq.head 
                  sprintf "Sending %A to connection %A with a weight of %A" data connectionId connection.Weight |> infoLog
                  data |> sendSynapseToNeuron connection.Neuron connectionId connection.Weight
                  let newDataStream = (dataStream |> Seq.tail)
                  processSensorSync newDataStream (remainingConnections |> Seq.tail)
              processSensorSync dataStream outboundConnectionsSeq
              return! loop barrier inboundConnections outboundConnections
          | ReceiveInput (neuronConnectionId, package) ->
            let updatedBarrier : IncomingSynapses =
              barrier
              |> Map.add neuronConnectionId package
            match neuronType with
            | Neuron props ->
              if updatedBarrier |> isBarrierSatisifed inboundConnections then
                sprintf "Barrier not satisfied for Node %A. Received %A from %A" props.Record.NodeId package neuronConnectionId |> infoLog
                neuronType |> activateNeuron updatedBarrier outboundConnections
                return! loop Map.empty inboundConnections outboundConnections
              else
                sprintf "Barrier not satisfied for Node %A. Received %A from %A" props.Record.NodeId package neuronConnectionId |> infoLog
                return! loop updatedBarrier inboundConnections outboundConnections
            | Actuator props ->
              if updatedBarrier |> isBarrierSatisifed inboundConnections then
                sprintf "Barrier is satisifed for Node %A" props.Record.NodeId |> infoLog
                neuronType |> activateNeuron updatedBarrier outboundConnections
                return! loop Map.empty inboundConnections outboundConnections
              else
                sprintf "Node %A not activated. Received %A from %A" props.Record.NodeId package neuronConnectionId |> infoLog
                return! loop updatedBarrier inboundConnections outboundConnections
            | Sensor _ ->
              //Sensors use the sync msg
              return! loop Map.empty inboundConnections outboundConnections
          | AddOutboundConnection ((toNode,nodeId,outboundLayer,weight),replyChannel) ->
              let neuronConnectionId =
                if (outboundConnections |> Map.isEmpty) then
                  1
                else
                  outboundConnections
                  |> Map.toSeq
                  |> Seq.maxBy (fun (connectionId,_) -> connectionId)
                  |> (fun x -> (x |> fst) + 1)
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
                (neuronConnectionId, (nodeId,0.0,weight))
                |> ReceiveInput
                |> toNode.Post

              sprintf "Node %A is adding Node %A as an outbound connection %A with weight %A" neuronType nodeId neuronConnectionId weight
              |> infoLog
              return! loop barrier inboundConnections updatedOutboundConnections
            | AddInboundConnection (neuronConnectionId,replyChannel) ->
              let updatedInboundConnections =
                inboundConnections |> Seq.append(Seq.singleton neuronConnectionId)
              replyChannel.Reply()
              sprintf "Added inbound neuron connection %A" neuronConnectionId
              |> infoLog
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
            | Die replyChannel ->
              replyChannel.Reply()
              return! loop barrier inboundConnections outboundConnections

      }
    loop Map.empty Seq.empty Map.empty
  )

  (nodeId, (nodeLayer, neuronInstance))
