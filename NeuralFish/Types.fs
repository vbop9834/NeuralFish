module NeuralFish.Types

type NeuronId = int

type ActivationFunctionId = int
type SyncFunctionId = int
type OutputHookId = int

type Bias = float
type Weight = float
type NeuronOutput = float
type ActuatorOutput = float
type SensorOutput = float seq

type ActuatorOutputMap = Map<OutputHookId, ActuatorOutput>

type ActivationFunction = NeuronOutput -> NeuronOutput
type SyncFunction = unit -> SensorOutput
type OutputHookFunction = ActuatorOutput -> unit

type ActivationFunctions = Map<ActivationFunctionId,ActivationFunction>
type SyncFunctions = Map<SyncFunctionId,SyncFunction>
type OutputHookFunctions = Map<OutputHookId, OutputHookFunction>

type OutputHookFunctionIds = OutputHookId seq
type SyncFunctionIds = SyncFunctionId seq

type NeuronLayerId = int

type NeuronConnectionId = System.Guid

type ConnectionOrder = int

type InboundNeuronConnection =
  {
    ConnectionOrder : ConnectionOrder option
    NeuronConnectionId : NeuronConnectionId
    FromNodeId : NeuronId
    Weight : Weight
    InitialWeight : Weight
  }

type Synapse = NeuronOutput
type WeightedSynapse = Synapse*InboundNeuronConnection
type WeightedSynapses = seq<WeightedSynapse>

type AxonHillockBarrier = Map<NeuronConnectionId, Synapse>

type IncomingSynapses = Map<NeuronConnectionId, Synapse>

type InactiveNeuronConnection =
  {
    NodeId : NeuronId
    Weight : Weight
    ConnectionOrder : ConnectionOrder option
  }

type NumberOfOutboundConnections = int

type NodeRecordType =
  | Neuron
  | Sensor of NumberOfOutboundConnections
  | Actuator

type NodeRecordConnections = Map<NeuronConnectionId,InactiveNeuronConnection>

type LearningRateCoefficient = float

type NeuronLearningAlgorithm =
  | Hebbian of LearningRateCoefficient
  | NoLearning

type NodeRecord =
  {
    Layer: NeuronLayerId
    NodeId: NeuronId
    NodeType: NodeRecordType
    InboundConnections: NodeRecordConnections
    Bias: Bias option
    ActivationFunctionId: ActivationFunctionId option
    SyncFunctionId: SyncFunctionId option
    OutputHookId: OutputHookId option
    MaximumVectorLength: int option
    NeuronLearningAlgorithm : NeuronLearningAlgorithm
  }

type NodeRecords = Map<NeuronId,NodeRecord>

type InfoLogFunction = string -> unit

type ConstructNeuralNetworkProperties =
  {
    ActivationFunctions : ActivationFunctions
    SyncFunctions : SyncFunctions
    OutputHooks : OutputHookFunctions
    NodeRecords : NodeRecords
    InfoLog : InfoLogFunction
  }

type ThinkCycleState =
  | ThinkCycleFinished
  | ThinkCycleIncomplete

type CortexMessage =
  | ThinkAndAct of AsyncReplyChannel<ThinkCycleState>
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

type PartialOutboundConnection =
  {
    ConnectionOrderOption : ConnectionOrder option
    InitialWeight : Weight
    ToNodeId : NeuronId
  }

type NeuronActivationOption =
  | ActivateIfBarrierIsFull
  | ActivateIfNeuronHasOneConnection
  | DoNotActivate

type NeuronActions =
  | Sync
  | ReceiveInput of NeuronConnectionId*Synapse*NeuronActivationOption
  | AddOutboundConnection of (NodeRecordType*MailboxProcessor<NeuronActions>*NeuronLayerId*PartialOutboundConnection)*AsyncReplyChannel<unit>
  | AddInboundConnection of InboundNeuronConnection*AsyncReplyChannel<unit>
  | GetNodeRecord of AsyncReplyChannel<NodeRecord>
  | Die of AsyncReplyChannel<unit>
  | RegisterCortex of CortexInstance*AsyncReplyChannel<unit>
  | ActivateActuator of AsyncReplyChannel<unit>
  | CheckActuatorStatus of AsyncReplyChannel<bool>
  | ResetNeuron of AsyncReplyChannel<unit>
  | SendRecurrentSignals of AsyncReplyChannel<unit>

type NeuronInstance = MailboxProcessor<NeuronActions>

type NeuronConnection =
  {
    NeuronConnectionId : NeuronConnectionId
    ConnectionOrder : ConnectionOrder
    InitialWeight : Weight
    Neuron: NeuronInstance
    NodeId: NeuronId
  }

type NeuronConnections = seq<NeuronConnection>
type RecurrentNeuronConnections = NeuronConnections

type InboundNeuronConnections = seq<InboundNeuronConnection>

type NeuralNetwork = Map<NeuronId, NeuronLayerId*NeuronInstance>

type Score = float

type EndGenerationOption =
  | EndGeneration
  | ContinueGeneration

type ScoreKeeperMsg =
  | Gather of AsyncReplyChannel<unit>*OutputHookId*float
  | GetScore of AsyncReplyChannel<float*EndGenerationOption>
  | KillScoreKeeper of AsyncReplyChannel<unit>

type ScoreKeeperInstance = MailboxProcessor<ScoreKeeperMsg>

type NodeRecordsId = int

type ScoredNodeRecords = array<NodeRecordsId*(Score*NodeRecords)>

type NeuralOutputs = Map<NeuronId, ActuatorOutput>

type FitnessFunction = NodeRecordsId -> NeuralOutputs -> Score*EndGenerationOption

type ThinkCycleOption =
  | EndThinkCycle
  | ContinueThinkCycle

type LiveFitnessFunction = NodeRecordsId -> ThinkCycleState -> Score*ThinkCycleOption

type GenerationRecords = Map<NodeRecordsId, NodeRecords>

type EndOfGenerationFunction = ScoredNodeRecords -> unit
type BeforeGenerationFunction = GenerationRecords -> unit

type SyncFunctionSource = NodeRecordsId -> SyncFunction
type SyncFunctionSources = Map<SyncFunctionId, SyncFunctionSource>

type Mutation =
  | MutateActivationFunction
  | AddBias
  | RemoveBias
  | MutateWeights
  | ResetWeights
  | AddInboundConnection
  | AddOutboundConnection
  | AddNeuron
  | AddNeuronOutSplice
  | AddNeuronInSplice
  | AddSensorLink
  | AddActuatorLink
  | RemoveSensorLink
  | RemoveActuatorLink
  | AddSensor
  | AddActuator
  | RemoveInboundConnection
  | RemoveOutboundConnection
  // | RemoveNeuron
  // //Remove neuron then connect inputs to outputs
  // | DespliceOut
  // | DespliceIn
  // | RemoveSensor
  // | RemoveActuator

type MaximumThinkCycles = int
type MaximumMinds = int
type AmountOfGenerations = int

type MutationSequence = Mutation seq

type FitPopulationSelectionFunction = ScoredNodeRecords -> GenerationRecords

type MutationProperties =
 {
   Mutations : MutationSequence
   ActivationFunctionIds : ActivationFunctionId seq
   SyncFunctionIds : SyncFunctionId seq
   OutputHookFunctionIds : OutputHookId seq
   LearningAlgorithm : NeuronLearningAlgorithm
   NodeRecords : NodeRecords
   InfoLog : InfoLogFunction
 }

type EvolutionProperties =
  {
    MaximumMinds : MaximumThinkCycles
    MaximumThinkCycles : MaximumMinds
    Generations : AmountOfGenerations
    MutationSequence : MutationSequence
    FitnessFunction : FitnessFunction
    ActivationFunctions : ActivationFunctions
    SyncFunctionSources : SyncFunctionSources
    OutputHookFunctionIds : OutputHookFunctionIds
    EndOfGenerationFunctionOption : EndOfGenerationFunction option
    StartingRecords : GenerationRecords
    NeuronLearningAlgorithm : NeuronLearningAlgorithm
    DividePopulationBy : int
    InfoLog : InfoLogFunction
    AsynchronousScoring : bool
    ThinkTimeout : int
  }

type DataGeneratorMsg<'T> =
  | GetData of AsyncReplyChannel<float seq>*NodeRecordsId
  | GetExpectedResult of AsyncReplyChannel<'T>*NodeRecordsId
  | ClearBuffer of AsyncReplyChannel<unit>
  | KillDataGenerator
type DataGeneratorInstance<'T> = MailboxProcessor<DataGeneratorMsg<'T>>

type TrainingAnswerAndDataSet<'T> = ('T*SensorOutput) array

type InterpretActuatorOutputFunction<'T> = ActuatorOutputMap -> 'T

//First 'T is correct Answer
//Second 'T is neural network guessed answer
type ScoreNeuralNetworkAnswerFunction<'T> = 'T -> 'T -> Score

type TrainingProperties<'T> =
  {
    AmountOfGenerations : AmountOfGenerations
    MaximumThinkCycles : MaximumThinkCycles
    MaximumMinds : MaximumMinds
    ActivationFunctions : ActivationFunctions
    OutputHookFunctionIds : OutputHookFunctionIds
    EndOfGenerationFunctionOption : EndOfGenerationFunction option
    StartingRecords : GenerationRecords
    MutationSequence : MutationSequence
    TrainingAnswerAndDataSet : TrainingAnswerAndDataSet<'T>
    InterpretActuatorOutputFunction : InterpretActuatorOutputFunction<'T>
    ScoreNeuralNetworkAnswerFunction : ScoreNeuralNetworkAnswerFunction<'T>
    NeuronLearningAlgorithm : NeuronLearningAlgorithm
    ShuffleDataSet : bool
    DividePopulationBy : int
    InfoLog : InfoLogFunction
  }

type LiveEvolutionMsg =
  | SynchronizeActiveCortex of AsyncReplyChannel<unit>
  | EndEvolution of AsyncReplyChannel<ScoredNodeRecords>

type LiveEvolutionInstance = MailboxProcessor<LiveEvolutionMsg>

type ActiveCortexBuffer = Score array

type LiveEvolutionProperties =
  {
    StarterRecords : GenerationRecords
    MutationSequence : MutationSequence
    NeuronLearningAlgorithm : NeuronLearningAlgorithm
    FitnessFunction : LiveFitnessFunction
    FitPopulationSelectionFunction : FitPopulationSelectionFunction
    MaximumMindsPerGeneration : MaximumMinds
    MaximumThinkCycles : MaximumThinkCycles option
    SyncFunctions : SyncFunctions
    OutputHookFunctions : OutputHookFunctions
    ActivationFunctions : ActivationFunctions
    EndOfGenerationFunctionOption : EndOfGenerationFunction option
    BeforeGenerationFunctionOption : BeforeGenerationFunction option
    InfoLog : InfoLogFunction
    ThinkTimeout : int
  }
