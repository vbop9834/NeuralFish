module NeuralFish.Types

exception NodeRecordTypeException of string
exception NeuronInstanceException of string
exception NoBiasInRecordForNeuronException of string
exception SensorRecordDoesNotHaveASyncFunctionException of string

type NeuronId = int

type ActivationFunctionId = int
type SyncFunctionId = int
type OutputHookId = int

type Bias = float
type Weight = float
type NeuronOutput = float
type ActuatorOutput = float

type ActivationFunction = NeuronOutput -> NeuronOutput
type SyncFunction = unit -> NeuronOutput seq
type OutputHookFunction = ActuatorOutput -> unit

type ActivationFunctions = Map<ActivationFunctionId,ActivationFunction>
type SyncFunctions = Map<SyncFunctionId,SyncFunction>
type OutputHookFunctions = Map<OutputHookId, OutputHookFunction>

type OutputHookFunctionIds = OutputHookId seq

type NeuronLayerId = float

type NeuronConnectionId = System.Guid

type Synapse = NeuronId*NeuronOutput*Weight

type AxonHillockBarrier = Map<NeuronConnectionId,Synapse>

type IncomingSynapses = Map<NeuronConnectionId, Synapse>

type InactiveNeuronConnection = NeuronId*Weight

type NodeRecordType =
  | Neuron
  | Sensor
  | Actuator

type NodeRecordConnections = Map<NeuronConnectionId,InactiveNeuronConnection>

type NodeRecord =
  {
    Layer: NeuronLayerId
    NodeId: NeuronId
    NodeType: NodeRecordType
    OutboundConnections: NodeRecordConnections
    Bias: Bias option
    ActivationFunctionId: ActivationFunctionId option
    SyncFunctionId: SyncFunctionId option
    OutputHookId: OutputHookId option
    MaximumVectorLength: int option
  }

type NodeRecords = Map<NeuronId,NodeRecord>

type CortexMessage =
    | Think of AsyncReplyChannel<unit>
    | KillCortex of AsyncReplyChannel<NodeRecords>

type CortexInstance = MailboxProcessor<CortexMessage>

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
  | ReceiveInput of NeuronConnectionId*Synapse*bool
  | AddOutboundConnection of (MailboxProcessor<NeuronActions>*NeuronId*NeuronLayerId*Weight)*AsyncReplyChannel<unit>
  | AddInboundConnection of NeuronConnectionId*AsyncReplyChannel<unit>
  | GetNodeRecord of AsyncReplyChannel<NodeRecord>
  | Die of AsyncReplyChannel<unit>
  | RegisterCortex of CortexInstance*AsyncReplyChannel<unit>
  | ActivateActuator of AsyncReplyChannel<unit>
  | CheckActuatorStatus of AsyncReplyChannel<bool>

type NeuronInstance = MailboxProcessor<NeuronActions>

type NeuronConnection =
  {
    Neuron: NeuronInstance
    NodeId: NeuronId
    Weight: Weight
  }

type NeuronConnections = Map<NeuronConnectionId,NeuronConnection>
type RecurrentNeuronConnections = Map<NeuronConnectionId,NeuronConnection>
type InboundNeuronConnections = NeuronConnectionId seq

type NeuralNetwork = Map<NeuronId, NeuronLayerId*NeuronInstance>

type Score = float

type ScoreKeeperMsg =
  | Gather of AsyncReplyChannel<unit>*OutputHookId*float
  | GetScore of AsyncReplyChannel<float>
  | KillScoreKeeper of AsyncReplyChannel<unit>

type ScoreKeeperInstance = MailboxProcessor<ScoreKeeperMsg>

type NodeRecordsId = int

type ScoredNodeRecords = array<NodeRecordsId*(Score*NodeRecords)>

type NeuralOutputs = Map<NeuronId, ActuatorOutput>

type FitnessFunction = NodeRecordsId -> NeuralOutputs -> Score

type GenerationRecords = Map<NodeRecordsId, NodeRecords>

type EndOfGenerationFunction = ScoredNodeRecords -> unit

type SyncFunctionSource = NodeRecordsId -> SyncFunction
type SyncFunctionSources = Map<SyncFunctionId, SyncFunctionSource>
