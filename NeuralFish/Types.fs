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

type NeuronConnectionId = System.Guid

type Synapse = NeuronId*NeuronOutput*Weight

type IncomingSynapses = Map<NeuronConnectionId, Synapse>

type InactiveNeuronConnection = NeuronId*Weight

type NeuronProperties =
  {
    id: NeuronId
    bias: Bias
    activationFunction: ActivationFunction
    activationFunctionId: ActivationFunctionId
  }

type SensorProperties =
  {
    id: NeuronId
    syncFunction: SyncFunction
    syncFunctionId: SyncFunctionId
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

type NeuronActions =
  | Sync
  | ReceiveInput of NeuronConnectionId*Synapse
  | AddOutboundConnection of (MailboxProcessor<NeuronActions>*NeuronId*Weight)*AsyncReplyChannel<unit>
  | AddInboundConnection of NeuronConnectionId*AsyncReplyChannel<unit>
  | GetNeuronTypeAndOutboundConnections of AsyncReplyChannel<NeuronType*seq<NeuronId*Weight>>

type NeuronInstance = MailboxProcessor<NeuronActions>

type NeuronConnection =
  {
    Neuron: NeuronInstance
    NodeId: NeuronId
    Weight: Weight
  }

type NeuronConnections = Map<NeuronConnectionId,NeuronConnection>
type InboundNeuronConnections = NeuronConnectionId seq
