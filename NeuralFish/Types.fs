module NeuralFish.Types

type NeuronId = int

type ActivationFunctionId = int
type SyncFunctionId = int
type OutputHookId = int

type Bias = float
type Weight = float
type NeuronOutput = float

type ActivationFunction = NeuronOutput -> NeuronOutput
type SyncFunction = unit -> NeuronOutput seq
type OutputHookFunction = NeuronOutput -> unit

type Synapse = NeuronId*NeuronOutput*Weight

type NeuronActions =
  | Sync
  | ReceiveInput of Synapse
  | IncrementBarrierThreshold of AsyncReplyChannel<unit>
  | AddOutboundConnection of MailboxProcessor<NeuronActions>*NeuronId*Weight

type NeuronInstance = MailboxProcessor<NeuronActions>

type NeuronConnection =
  {
    nodeId: NeuronId
    neuron: NeuronInstance
    weight: Weight
  }

type NeuronConnections = NeuronConnection seq

type NeuronProperties =
  {
    id: NeuronId
    bias: Bias
    activationFunction: ActivationFunction
    activationFunctionId: ActivationFunctionId
    outbound_connections: NeuronConnections
  }

type SensorProperties =
  {
    id: NeuronId
    syncFunction: SyncFunction
    syncFunctionId: SyncFunctionId
    outbound_connections:  NeuronConnections
  }

type ActuatorProperties =
  {
    id: NeuronId
    outputHook: OutputHookFunction
    outputHookId: OutputHookId
  }

type NeuronType =
  | Neuron of NeuronProperties
  | Sensor of SensorProperties
  | Actuator of ActuatorProperties

type NeuronIdGeneratorMsg =
  | GetNeuronId of AsyncReplyChannel<NeuronId>
