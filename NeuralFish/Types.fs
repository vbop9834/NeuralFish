module NeuralFish.Types

exception NodeRecordTypeException of string
exception NeuronInstanceException of string
exception NoBiasInRecordForNeuronException of string

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

type NodeRecordType =
  | Neuron
  | Sensor
  | Actuator

type NodeRecordConnections = Map<NeuronConnectionId,InactiveNeuronConnection>

type NodeRecord =
  {
    NodeId: NeuronId
    NodeType: NodeRecordType
    OutboundConnections: NodeRecordConnections
    Bias: Bias option
    ActivationFunctionId: ActivationFunctionId option
    SyncFunctionId: ActivationFunctionId option
    OutputHookId: OutputHookId option
  }

type NodeRecords = Map<NeuronId,NodeRecord>

type NeuronProperties =
  {
    ActivationFunction: ActivationFunction
    Record: NodeRecord
  }

type SensorProperties =
  {
    SyncFunction: SyncFunction
    Record: NodeRecord
  }

type ActuatorProperties =
  {
    OutputHook: OutputHookFunction
    Record: NodeRecord
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
  | GetNodeRecord of AsyncReplyChannel<NodeRecord>

type NeuronInstance = MailboxProcessor<NeuronActions>

type NeuronConnection =
  {
    Neuron: NeuronInstance
    NodeId: NeuronId
    Weight: Weight
  }

type NeuronConnections = Map<NeuronConnectionId,NeuronConnection>
type InboundNeuronConnections = NeuronConnectionId seq

type NeuralNetwork = Map<NeuronId, NeuronInstance>
