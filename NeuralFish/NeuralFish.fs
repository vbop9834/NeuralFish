module NeuralFish.Core

open NeuralFish.Types

let synchronize (_, (sensor : NeuronInstance)) =
  Sync |> sensor.Post

let synapseDotProduct synapses =
  let rec loop synapses =
    match synapses with
    | [] -> 0.0
    | (_,value,weight)::tail -> value*weight + (loop tail)
  synapses |> Seq.toList |> loop

let createNeuron id activationFunction activationFunctionId bias =
  {
    id = id
    bias = bias
    activationFunction = activationFunction
    activationFunctionId = activationFunctionId
    outbound_connections = Seq.empty
  } |> Neuron
let createSensor id syncFunction syncFunctionId =
  {
    id = id
    syncFunction = syncFunction
    syncFunctionId = syncFunctionId
    outbound_connections = Seq.empty
  } |> Sensor
let createActuator id outputHook outputHookId =
  {
    id = id
    outputHook = outputHook
    outputHookId = outputHookId
  } |> Actuator

let private addNodeToConnections weight toNode nodeId outbound_connections =
  let addConnection connectionSet connection =
    IncrementBarrierThreshold |> connection.neuron.PostAndReply
    connectionSet |> Seq.append (Seq.singleton connection)
  let neuronConnection =
    {
      nodeId = nodeId
      weight = weight
      neuron = toNode
    }
  neuronConnection |> addConnection outbound_connections

let connectNodeToNeuron weight (nodeId, toNode) fromNode  =
  match fromNode with
    | Neuron props ->
      let newOutboundConnections = addNodeToConnections weight toNode nodeId props.outbound_connections
      Neuron <| { props with outbound_connections = newOutboundConnections  }
    | Sensor props ->
      let newOutboundConnections = addNodeToConnections weight toNode nodeId props.outbound_connections
      Sensor <| { props with outbound_connections = newOutboundConnections }
    | Actuator props -> fromNode

let connectSensorToNode weights (nodeId, toNode) fromNode =
  match fromNode with
    | Sensor props ->
      let rec getConnectionsFromWeights weights connections =
        if (weights |> Seq.isEmpty) then
          connections
        else
          let weight = weights |> Seq.head
          let weights = weights |> Seq.tail
          let newOutboundConnections = addNodeToConnections weight toNode props.id connections
          getConnectionsFromWeights weights newOutboundConnections
      let newOutboundConnections = getConnectionsFromWeights weights props.outbound_connections
      Sensor <| { props with outbound_connections = newOutboundConnections }
    | _ -> connectNodeToNeuron (weights |> Seq.head) (nodeId, toNode) fromNode

let connectNodeToActuator toNode fromNode =
  fromNode |> connectNodeToNeuron 0.0 toNode

let createNeuronInstance neuronType =
  let getNodeIdFromProps neuronType =
    match neuronType with
      | Neuron props ->
        props.id
      | Sensor props ->
        props.id
      | Actuator props ->
        props.id
  let isBarrierSatisifed barrierThreshold barrier =
    barrierThreshold = (barrier |> Seq.length)
  let sendSynapseToNeurons (outputNeurons : NeuronConnection seq ) partialSynapse =
    let sendSynapseToNeuron (nodeId, outputValue) outputNeuronConnection =
      (nodeId, outputValue, outputNeuronConnection.weight)
      |> ReceiveInput
      |> outputNeuronConnection.neuron.Post
    outputNeurons
    |> Seq.iter (sendSynapseToNeuron partialSynapse)
  let addBias bias outputVal =
    outputVal + bias
  let activateNeuron barrier connections neuronType =
    match neuronType with
    | Neuron props ->
        barrier
        |> synapseDotProduct
        |> addBias props.bias
        |> (fun outputValue -> (props.id, (props.activationFunction outputValue)))
        |> sendSynapseToNeurons connections
        Seq.empty
    | Actuator props ->
        barrier
        |> Seq.sumBy(fun (_,value,weight) -> value)
        |> props.outputHook
        Seq.empty
    | Sensor _ ->
        barrier

  let connections =
    match neuronType with
      | Neuron props ->
        props.outbound_connections
      | Sensor props ->
        props.outbound_connections
      | Actuator _ ->
        Seq.empty

  let neuronInstance = NeuronInstance.Start(fun inbox ->
    let rec loop barrier barrierThreshold connections =
      async {
        let! msg = inbox.Receive ()
        match msg with
        | Sync ->
          match neuronType with
          | Neuron _ ->
            return! loop barrier barrierThreshold connections
          | Actuator _ ->
            return! loop barrier barrierThreshold connections
          | Sensor props ->
            let rec processSensorSync connections dataStream =
              let sendSynapseToNeuron outboundNeuronConnection (nodeId, outputValue)  =
                (nodeId, outputValue, outboundNeuronConnection.weight)
                |> ReceiveInput
                |> outboundNeuronConnection.neuron.Post
              if (connections |> Seq.isEmpty) then
                ()
              else
                let data = dataStream |> Seq.head
                let dataStream = (dataStream |> Seq.tail) |> Seq.append(Seq.singleton data)
                let connection = connections |> Seq.head
                let tailConnections = connections |> Seq.tail
                (props.id, data) |> sendSynapseToNeuron connection
                processSensorSync tailConnections dataStream
            props.syncFunction()
            |> processSensorSync connections
            return! loop barrier barrierThreshold connections
        | ReceiveInput package ->
          match neuronType with
          | Neuron props ->
            let barrier =
              barrier
              |> Seq.append (Seq.singleton package)
            if barrier |> isBarrierSatisifed barrierThreshold then
              let barrier = neuronType |> activateNeuron barrier connections
              return! loop barrier barrierThreshold connections
            else
              return! loop barrier barrierThreshold connections
          | Actuator props ->
            let barrier =
             barrier
             |> Seq.append (Seq.singleton package)
            if barrier |> isBarrierSatisifed barrierThreshold then
              let barrier = neuronType |> activateNeuron barrier Seq.empty
              return! loop Seq.empty barrierThreshold connections
            else
              return! loop barrier barrierThreshold connections
          | Sensor _ ->
            //Sensors use the sync msg
            return! loop barrier barrierThreshold connections
        | IncrementBarrierThreshold replyChannel ->
          replyChannel.Reply()
          let barrierThreshold = barrierThreshold + 1
          return! loop barrier barrierThreshold connections
        | AddOutboundConnection (toNode,nodeId,weight) ->
          let connections =
            {
              nodeId = nodeId
              neuron = toNode
              weight = weight
            } |> (fun connection -> connections |> Seq.append(Seq.singleton connection))
          let barrierThreshold = barrierThreshold + 1
          return! loop barrier barrierThreshold connections
      }
    loop Seq.empty 0 connections
  )

  (neuronType, (neuronType |> getNodeIdFromProps, neuronInstance))
