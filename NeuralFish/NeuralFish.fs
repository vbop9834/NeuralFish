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
    inbound_connections: NeuronConnection seq
    outbound_connections: NeuronConnection seq
  }

type SensorProperties =
  {
    syncFunction: unit -> float
    outbound_connections:  NeuronConnection seq
  }

type ActuatorProperties =
  {
    inbound_connections: NeuronConnection seq
    outbound_connections: NeuronConnection seq
  }

type NeuronType =
  | Neuron of NeuronProperties
  | Sensor of SensorProperties
  | Actuator of ActuatorProperties

let createNeuron neuronType =
  let isBarrierSatisifed outputConnections barrier =
    (outputConnections |> Seq.length) = (barrier |> Seq.length)
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
            |> sendSynapseToNeurons props.outbound_connections
          | Sensor _ ->
              return! loop barrier
        | ReceiveInput package ->
          match neuronType with
          | Neuron props ->
            if barrier |> isBarrierSatisifed props.outbound_connections then
              Activate |> inbox.Post
              return! loop Seq.empty
            else
              let barrier =
                barrier
                |> Seq.append (Seq.singleton (package.weight * package.value))
              return! loop barrier
          | Actuator props ->
            if barrier |> isBarrierSatisifed props.outbound_connections then
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
