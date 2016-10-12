module NeuralFish

type Synapse =
  {
    value: float
    weight: float
  }

type NeuronActions =
  | Sync
  | Activate
  | ReceiveInput of Synapse

type NeuronInstance = MailboxProcessor<NeuronActions>

type NeuronConnection =
  {
    neuron: NeuronInstance
    weight: float
  }

type NeuronProperties =
  {
    bias: float
    activationFunction: float -> float
    barrierThreshold: int
    outbound_connections: NeuronConnection seq
  }

type SensorProperties =
  {
    syncFunction: unit -> float
    outbound_connections:  NeuronConnection seq
  }

type ActuatorProperties =
  {
    barrierThreshold: int
    outputHook: float -> unit
  }

type NeuronType =
  | Neuron of NeuronProperties
  | Sensor of SensorProperties
  | Actuator of ActuatorProperties

let createNeuron activationFunction bias =
  {
    bias = bias
    activationFunction = activationFunction
    barrierThreshold = 0
    outbound_connections = Seq.empty
  } |> Neuron
let createSensor syncFunction =
  {
    syncFunction = syncFunction
    outbound_connections = Seq.empty
  } |> Sensor
let createActuator outputHook =
  {
    barrierThreshold = 0
    outputHook = outputHook
  } |> Actuator

let connectNodeToNeuron weight toNode fromNode  =
  let addConnection connectionSet connection =
    connectionSet |> Seq.append (Seq.singleton connection)

  match fromNode with
    | Neuron props ->
      let neuronConnection =
        {
          weight = weight
          neuron = toNode
        }
      Neuron <|  { props with outbound_connections = neuronConnection |> addConnection props.outbound_connections }
    | Sensor props ->
      let neuronConnection =
        {
          weight = 0.0
          neuron = toNode
        }
      Sensor <|  { props with outbound_connections = neuronConnection |> addConnection props.outbound_connections }
    | Actuator props -> fromNode

let connectNodeTo toNode fromNode =
  fromNode |> connectNodeToNeuron 0.0 toNode

let createNeuronInstance neuronType =
  let isBarrierSatisifed barrierThreshold barrier =
    barrierThreshold = (barrier |> Seq.length)
  let sendSynapseToNeurons (outputNeurons : NeuronConnection seq ) outputValue =
    let sendSynapseToNeuron outputValue outputNeuronConnection =
      { value = outputValue; weight = outputNeuronConnection.weight }
      |> ReceiveInput
      |> outputNeuronConnection.neuron.Post
    outputNeurons
    |> Seq.iter (sendSynapseToNeuron outputValue)
  let addBias bias outputVal =
    outputVal + bias

  NeuronInstance.Start(fun inbox ->
    let rec loop barrier =
      async {
        let! msg = inbox.Receive ()
        match msg with
        | Sync ->
          match neuronType with
          | Neuron _ ->
            return! loop barrier
          | Actuator _ ->
            return! loop barrier
          | Sensor props ->
            let outputVal = props.syncFunction()
            outputVal |> sendSynapseToNeurons props.outbound_connections
            return! loop barrier
        | Activate ->
          match neuronType with
          | Neuron props ->
            barrier
            |> Seq.sum
            |> addBias props.bias
            |> props.activationFunction
            |> sendSynapseToNeurons props.outbound_connections
          | Actuator props ->
            barrier
            |> Seq.sum
            |> props.outputHook
          | Sensor _ ->
              return! loop barrier
        | ReceiveInput package ->
          match neuronType with
          | Neuron props ->
            if barrier |> isBarrierSatisifed props.barrierThreshold then
              Activate |> inbox.Post
              return! loop Seq.empty
            else
              let barrier =
                barrier
                |> Seq.append (Seq.singleton (package.weight * package.value))
              return! loop barrier
          | Actuator props ->
            if barrier |> isBarrierSatisifed props.barrierThreshold then
              Activate |> inbox.Post
              return! loop Seq.empty
            else
              let barrier =
                barrier
                |> Seq.append (Seq.singleton package.value)
              return! loop barrier
          | Sensor _ ->
            //Sensors use the sync msg
            return! loop barrier
      }
    loop Seq.empty
  )
