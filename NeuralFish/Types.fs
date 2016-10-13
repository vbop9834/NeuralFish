module NeuralFish.Types

//value*weight
type Synapse = int*float*float

type NeuronActions =
  | Sync
  | ReceiveInput of Synapse
  | IncrementBarrierThreshold of AsyncReplyChannel<unit>

type NeuronInstance = MailboxProcessor<NeuronActions>

type NeuronConnection =
  {
    nodeId: int
    neuron: NeuronInstance
    weight: float
  }

type NeuronProperties =
  {
    id: int
    bias: float
    activationFunction: float -> float
    activationFunctionId: int
    outbound_connections: NeuronConnection seq
  }

type SensorProperties =
  {
    id: int
    syncFunction: unit -> float seq
    syncFunctionId: int
    outbound_connections:  NeuronConnection seq
  }

type ActuatorProperties =
  {
    id: int
    outputHook: float -> unit
    outputHookId: int
  }

type NeuronType =
  | Neuron of NeuronProperties
  | Sensor of SensorProperties
  | Actuator of ActuatorProperties

type NeuronIdGeneratorMsg =
  | GetNeuronId of AsyncReplyChannel<int>
