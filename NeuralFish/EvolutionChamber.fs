module NeuralFish.EvolutionChamber

open NeuralFish.Types
open NeuralFish.Exceptions
open NeuralFish.Core
open NeuralFish.Exporter
open NeuralFish.Cortex

let minimalMutationSequence : MutationSequence =
  [
    MutateActivationFunction
    AddBias
    RemoveBias
    MutateWeights
    AddInboundConnection
    AddOutboundConnection
    AddNeuron
    AddSensor
    AddActuator
    AddSensorLink
    AddActuatorLink
  ] |> List.toSeq

let mutateNeuralNetwork (mutationProperties : MutationProperties) : NodeRecords =
  let outputHookFunctionIds = mutationProperties.OutputHookFunctionIds
  let syncFunctionIds = mutationProperties.SyncFunctionIds
  let activationFunctionIds = mutationProperties.ActivationFunctionIds
  let mutations = mutationProperties.Mutations
  let learningAlgorithm = mutationProperties.LearningAlgorithm
  let nodeRecords = mutationProperties.NodeRecords
  let infoLog = mutationProperties.InfoLog

  let numberOfOutputHookFunctions = outputHookFunctionIds |> Seq.length
  let numberOfSyncFunctions = syncFunctionIds |> Seq.length
  let numberOfActivationFunctions = activationFunctionIds |> Seq.length
  let mutationSequenceLength = mutations |> Seq.length

  let random = System.Random()
  let getRandomDoubleBetween minValue maxValue =
    random.NextDouble() * (maxValue - minValue) + minValue
  let totalNumberOfMutations = mutations |> Seq.length
  let selectRandomMutation _ =
    mutations |> Seq.item (totalNumberOfMutations |> random.Next)
  let pendingMutations =
    let numberOfNodesInNodeRecords = nodeRecords |> Map.toSeq |> Seq.length
    let numberOfMutations =
      random.NextDouble() * (sqrt (numberOfNodesInNodeRecords |> float))
      |> System.Math.Ceiling
      |> int
    sprintf "Selecting %i number of mutations" numberOfMutations
    |> infoLog

    [1..numberOfMutations]
    |> Seq.map selectRandomMutation
  sprintf "Pending Mutations %A" pendingMutations |> infoLog
  let rec processMutationSequence pendingMutations processingNodeRecords =
    sprintf "Pending Mutations %A" pendingMutations |> infoLog
    if (pendingMutations |> Seq.isEmpty) then
      processingNodeRecords
    else
      let rec mutate mutation =
        sprintf "Mutating using %A" mutation |> infoLog
        let addInboundConnection (toNode : NodeRecord) (fromNode : NodeRecord) =
          let newInboundConnections =
            toNode.InboundConnections
            |> Map.add (System.Guid.NewGuid()) (toNode.NodeId,1.0)
          { toNode with InboundConnections = newInboundConnections }
        let selectRandomNode (randomNodeRecords : NodeRecords) =
          let seqOfNodeRecords = randomNodeRecords |> Map.toSeq
          let randomNumber =
            seqOfNodeRecords
            |> Seq.length
            |> random.Next
          seqOfNodeRecords
          |> Seq.item randomNumber
        let selectRandomActivationFunctionId () =
          let randomNumber =
            numberOfActivationFunctions
            |> random.Next
          activationFunctionIds
          |> Seq.item randomNumber
        let selectRandomSyncFunctionId inUseSyncFunctionIds =
          let availableIds =
            syncFunctionIds
            |> Seq.filter(fun syncFunctionId -> inUseSyncFunctionIds |> Seq.contains syncFunctionId |> not)
          let randomNumber =
            let numberOfAvailableIds =
              availableIds |> Seq.length
            numberOfAvailableIds
            |> random.Next
          availableIds
          |> Seq.item randomNumber
        let selectRandomOutputHookFunctionId inUseOutputHooks =
          let availableIds =
            outputHookFunctionIds
            |> Seq.filter(fun outputHookId ->  inUseOutputHooks |> Seq.contains outputHookId |> not)
          let randomNumber =
            let numberOfAvailableIds =
              availableIds |> Seq.length
            numberOfAvailableIds
            |> random.Next
          availableIds
          |> Seq.item randomNumber

        let mutateRandomly () =
          if (mutationSequenceLength = 1) then
            processingNodeRecords
          else
            selectRandomMutation () |> mutate

        match mutation with
        | MutateActivationFunction ->
          let _,neuronToMutate =
            processingNodeRecords
            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
            |> selectRandomNode
          let newActivationFunctionId = selectRandomActivationFunctionId ()
          let mutatedNeuron = { neuronToMutate with ActivationFunctionId = Some newActivationFunctionId }
          processingNodeRecords
          |> Map.add mutatedNeuron.NodeId mutatedNeuron
        | AddBias ->
          let _,neuronToAddBias =
            processingNodeRecords
            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
            |> selectRandomNode
          let addBiasToNeuronAndSaveToRecords (nodeRecord : NodeRecord) =
            let addRandomBiasToNeuron (neuron : NodeRecord) =
              let bias : Bias = random.NextDouble()
              sprintf "Adding bias %f to neuron %A" bias neuron.NodeId |> infoLog
              { neuron with Bias = Some bias }
            let updatedNodeRecord = nodeRecord |> addRandomBiasToNeuron
            processingNodeRecords
            |> Map.add updatedNodeRecord.NodeId updatedNodeRecord
          match neuronToAddBias.Bias with
          | Some bias ->
            if (bias = 0.0) then
              neuronToAddBias
              |> addBiasToNeuronAndSaveToRecords
            else
              if totalNumberOfMutations = 1 then
                processingNodeRecords
              else
                sprintf "Neuron %A already has bias %f" neuronToAddBias.NodeId bias |> infoLog
                mutateRandomly()
          | None ->
            neuronToAddBias
            |> addBiasToNeuronAndSaveToRecords
        | RemoveBias ->
          let _,neuronToRemoveBias =
            processingNodeRecords
            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
            |> selectRandomNode
          let removeBiasFromNeuronAndSaveRecords neuron =
            let removeBiasFromNeuron neuron =
              { neuron with Bias = None }
            let updatedNeuron = neuron |> removeBiasFromNeuron
            processingNodeRecords
            |> Map.add updatedNeuron.NodeId updatedNeuron
          match neuronToRemoveBias.Bias with
            | Some bias ->
              if (bias > 0.0) then
                neuronToRemoveBias
                |> removeBiasFromNeuronAndSaveRecords
              else
                if totalNumberOfMutations = 1 then
                  processingNodeRecords
                else
                  sprintf "Neuron %A already has no bias already" neuronToRemoveBias.NodeId|> infoLog
                  mutateRandomly()
            | None ->
              if totalNumberOfMutations = 1 then
                processingNodeRecords
              else
                sprintf "Neuron %A already has no bias already" neuronToRemoveBias.NodeId|> infoLog
                mutateRandomly()
        | MutateWeights ->
          let _, neuronToMutateWeights =
            processingNodeRecords
            |> Map.filter(fun _ x -> x.NodeType <> NodeRecordType.Actuator)
            |> selectRandomNode
          let mutatedNeuron =
            let probabilityOfWeightMutation =
              let totalNumberOfInboundCOnnections = neuronToMutateWeights.InboundConnections |> Map.toSeq |> Seq.length |> float
              1.0/(sqrt totalNumberOfInboundCOnnections)
            let newInboundConnections =
              let calculateProbabilityAndMutateWeight _ inactiveConnection =
                let mutateWeight ((nodeId, weight) : InactiveNeuronConnection) =
                  let newWeight =
                    let pi = System.Math.PI
                    let maxValue = pi/2.0
                    let minValue = -1.0 * pi/2.0
                    getRandomDoubleBetween minValue maxValue
                  (nodeId, newWeight)
                if random.NextDouble() <= probabilityOfWeightMutation then
                  inactiveConnection |> mutateWeight
                else
                  inactiveConnection
              neuronToMutateWeights.InboundConnections
              |> Map.map calculateProbabilityAndMutateWeight
            { neuronToMutateWeights with InboundConnections = newInboundConnections }

          processingNodeRecords
          |> Map.add mutatedNeuron.NodeId mutatedNeuron
       // | ResetWeights ->
//        | MutateActivationFunction ->
//          let neuronToMutateAF =
//            nodeRecords
//            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
//            |> selectRandomNode
        | Mutation.AddOutboundConnection
        | Mutation.AddInboundConnection ->
          let _,fromNode =
            processingNodeRecords
            |> Map.filter(fun _ x -> x.NodeType <> NodeRecordType.Actuator)
            |> selectRandomNode
          let _,toNode =
            processingNodeRecords
            |> Map.filter(fun _ x -> x.NodeType <> NodeRecordType.Sensor)
            |> selectRandomNode
          let mutatedNode =
            fromNode
            |> addInboundConnection toNode
          processingNodeRecords
          |> Map.add mutatedNode.NodeId mutatedNode
        | AddNeuron ->
          let _,fromNode =
            processingNodeRecords
            |> Map.filter(fun _ x -> x.NodeType <> NodeRecordType.Actuator)
            |> selectRandomNode
          let _,toNode =
            processingNodeRecords
            |> Map.filter(fun _ x -> x.NodeType <> NodeRecordType.Sensor)
            |> selectRandomNode
          let blankNewNeuronRecord =
            let seqOfNodes =
              processingNodeRecords
              |> Map.toSeq
            let layer =
              match fromNode.Layer < toNode.Layer with
              | true ->
                  getRandomDoubleBetween fromNode.Layer toNode.Layer
              | false ->
                  getRandomDoubleBetween fromNode.Layer toNode.Layer
            let inboundConnections = Map.empty
            let nodeId =
              seqOfNodes
              |> Seq.maxBy(fun (nodeId,_) -> nodeId)
              |> (fun (nodeId,_) -> nodeId + 1)
            let activationFunctionId = selectRandomActivationFunctionId ()

            {
              Layer = layer
              NodeId = nodeId
              NodeType = NodeRecordType.Neuron
              InboundConnections = inboundConnections
              Bias = None
              ActivationFunctionId = Some activationFunctionId
              SyncFunctionId = None
              OutputHookId = None
              MaximumVectorLength = None
              NeuronLearningAlgorithm = learningAlgorithm
            }
          let updatedToNode =
            blankNewNeuronRecord
            |> addInboundConnection toNode
          let updatedNeuronRecord =
            fromNode
            |> addInboundConnection blankNewNeuronRecord
          processingNodeRecords
          |> Map.add updatedNeuronRecord.NodeId updatedNeuronRecord
          |> Map.add updatedToNode.NodeId updatedToNode
       // | OutSplice ->
       // | InSplice ->
        | AddSensorLink ->
          let sensorRecordsThatCanHaveAnotherOutput =
            let determineSensorEligibility key (nodeRecord : NodeRecord) =
              if (nodeRecord.NodeType = NodeRecordType.Sensor) then
                let numberOfOutboundConnctions =
                  let getOutboundConnectionCount =
                    (fun (_,toNode) ->
                      toNode.InboundConnections
                      |> Map.toSeq
                      |> Seq.filter(fun (_,(fromNodeId,_)) -> fromNodeId = nodeRecord.NodeId)
                      |> Seq.length
                    )
                  processingNodeRecords
                  |> Map.toArray
                  |> Array.filter(fun (_, x) -> x.NodeType <> NodeRecordType.Sensor)
                  |> Array.Parallel.map getOutboundConnectionCount
                  |> Array.sum
                match nodeRecord.MaximumVectorLength with
                | None -> false
                | Some maximumVectorLength ->
                  if maximumVectorLength = 0 then
                    true
                  else
                    maximumVectorLength > numberOfOutboundConnctions
              else
                false
            processingNodeRecords
            |> Map.filter determineSensorEligibility
          if (sensorRecordsThatCanHaveAnotherOutput |> Map.isEmpty) then
            mutateRandomly ()
          else
            let _, sensorNode =
              sensorRecordsThatCanHaveAnotherOutput
              |> selectRandomNode
            let _, toNode =
              processingNodeRecords
              |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
              |> selectRandomNode
            let updatedToNode =
              sensorNode
              |> addInboundConnection toNode
            processingNodeRecords
            |> Map.add updatedToNode.NodeId updatedToNode
        | AddActuatorLink ->
          let _,fromNode =
            processingNodeRecords
            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
            |> selectRandomNode
          let _, toActuator =
            processingNodeRecords
            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Actuator)
            |> selectRandomNode
          let actuatorWithInbound =
            fromNode
            |> addInboundConnection toActuator
          processingNodeRecords
          |> Map.add actuatorWithInbound.NodeId actuatorWithInbound
       // | RemoveSensorLink ->
       // | RemoveActuatorLink ->
        | AddSensor ->
          let sensorRecords =
            processingNodeRecords
            |> Map.filter(fun _ record -> record.NodeType = NodeRecordType.Sensor)
          let numberOfCurrentSensors =
            sensorRecords
            |> Map.toSeq
            |> Seq.length
          match numberOfSyncFunctions > numberOfCurrentSensors with
          | false ->
            mutateRandomly()
          | true ->
            let _,outboundNode =
              processingNodeRecords
              |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
              |> selectRandomNode
            let blankSensorRecord =
              let layer = 0.0
              let inboundConnections = Map.empty
              let nodeId =
                processingNodeRecords
                |> Map.toSeq
                |> Seq.maxBy(fun (nodeId,_) -> nodeId)
                |> (fun (nodeId,_) -> nodeId + 1)
              let inUseSyncFunctions =
                let extractSyncFunctionId (_, record) =
                  match record.SyncFunctionId with
                  | Some syncFunctionId -> syncFunctionId
                  | None -> raise <| SensorRecordDoesNotHaveASyncFunctionException (sprintf "Record %A does not have a sync function" record)
                sensorRecords
                |> Map.toSeq
                |> Seq.map extractSyncFunctionId
              let syncFunctionId = selectRandomSyncFunctionId inUseSyncFunctions

              {
                Layer = layer
                NodeId = nodeId
                NodeType = NodeRecordType.Sensor
                InboundConnections = inboundConnections
                Bias = None
                ActivationFunctionId = None
                SyncFunctionId = Some syncFunctionId
                OutputHookId = None
                MaximumVectorLength = Some 1
                NeuronLearningAlgorithm = NoLearning
              }
            let updatedOutboundNode =
              blankSensorRecord
              |> addInboundConnection outboundNode
            processingNodeRecords
            |> Map.add blankSensorRecord.NodeId blankSensorRecord
            |> Map.add updatedOutboundNode.NodeId updatedOutboundNode
        | AddActuator ->
          let actuatorRecords =
            processingNodeRecords
            |> Map.filter(fun _ record -> record.NodeType = NodeRecordType.Actuator)
          let numberOfCurrentActuators =
            actuatorRecords
            |> Map.toSeq
            |> Seq.length
          match numberOfOutputHookFunctions > numberOfCurrentActuators with
          | false ->
            mutateRandomly()
          | true ->
            let _,inboundNode =
              processingNodeRecords
              |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
              |> selectRandomNode
            let blankActuatorRecord =
              let seqOfNodes =
                processingNodeRecords
                |> Map.toSeq
              let layer =
                let maxLayer =
                  seqOfNodes
                  |> Seq.maxBy(fun (_,nodeRecord) -> nodeRecord.Layer)
                  |> (fun (_,record) -> record.Layer)
                maxLayer
                |> round
              let inboundConnections = Map.empty
              let nodeId =
                seqOfNodes
                |> Seq.maxBy(fun (nodeId,_) -> nodeId)
                |> (fun (nodeId,_) -> nodeId + 1)

              let inUseOutputHooks =
                let extractOutputHookId (_, record) =
                  match record.OutputHookId with
                  | Some syncFunctionId -> syncFunctionId
                  | None -> raise <| ActuatorRecordDoesNotHaveAOutputHookIdException (sprintf "Record %A does not have a output hook function" record)
                actuatorRecords
                |> Map.toSeq
                |> Seq.map extractOutputHookId

              let outputHookId = selectRandomOutputHookFunctionId inUseOutputHooks

              {
                Layer = layer
                NodeId = nodeId
                NodeType = NodeRecordType.Actuator
                InboundConnections = inboundConnections
                Bias = None
                ActivationFunctionId = None
                SyncFunctionId = None
                OutputHookId = Some outputHookId
                MaximumVectorLength = None
                NeuronLearningAlgorithm = NoLearning
              }
            let newActuatorRecord =
              inboundNode
              |> addInboundConnection blankActuatorRecord
            processingNodeRecords
            |> Map.add newActuatorRecord.NodeId newActuatorRecord
       // | RemoveInboundConnection ->
       // | RemoveOutboundConnection ->
       // | RemoveNeuron ->
       // | DespliceOut ->
       // | RemoveSensor ->
       // | RemoveActuator ->
      pendingMutations
      |> Seq.head
      |> mutate
      |> processMutationSequence (pendingMutations |> Seq.tail)
  nodeRecords
  |> processMutationSequence pendingMutations

let defaultEvolutionProperties : EvolutionProperties =
  {
    MaximumMinds = 5
    MaximumThinkCycles = 5
    Generations = 5
    MutationSequence = minimalMutationSequence
    FitnessFunction = (fun _ _ -> 0.0, ContinueGeneration)
    ActivationFunctions = Map.empty
    SyncFunctionSources = Map.empty
    OutputHookFunctionIds = Seq.empty
    EndOfGenerationFunctionOption = None
    StartingRecords = Map.empty
    NeuronLearningAlgorithm = Hebbian 0.5
    DividePopulationBy = 2
    InfoLog = defaultInfoLog
    AsynchronousScoring = true
  }

let evolveForXGenerations (evolutionProperties : EvolutionProperties)
                   : ScoredNodeRecords =
  let activationFunctions = evolutionProperties.ActivationFunctions
  let syncFunctionSources = evolutionProperties.SyncFunctionSources
  let outputHookFunctionIds = evolutionProperties.OutputHookFunctionIds
  let maximumMinds = evolutionProperties.MaximumMinds
  let maximumThinkCycles = evolutionProperties.MaximumThinkCycles
  let fitnessFunction = evolutionProperties.FitnessFunction
  let infoLog = evolutionProperties.InfoLog

  let endOfGenerationFunction =
    match evolutionProperties.EndOfGenerationFunctionOption with
    | Some endOfGenerationFunction -> endOfGenerationFunction
    | None -> (fun _ -> ())
  let generations = evolutionProperties.Generations

  let mutationFunction =
    let mutationSequence = evolutionProperties.MutationSequence
    let activationFunctionIds =
      activationFunctions
      |> Map.toSeq
      |> Seq.map (fun (id,_) -> id)
    let syncFunctionIds =
      syncFunctionSources
      |> Map.toSeq
      |> Seq.map (fun (id,_) -> id)
    let completeMutationProperties (records : NodeRecords) : MutationProperties =
      {
        Mutations = mutationSequence
        ActivationFunctionIds = activationFunctionIds
        SyncFunctionIds = syncFunctionIds
        OutputHookFunctionIds = outputHookFunctionIds
        LearningAlgorithm = evolutionProperties.NeuronLearningAlgorithm
        InfoLog = evolutionProperties.InfoLog
        NodeRecords = records
      }
    (fun records -> records |> completeMutationProperties |> mutateNeuralNetwork )

  let evolveGeneration (generationRecords : GenerationRecords) : GenerationRecords =
    let processEvolution currentGen =
      let rec processEvolutionLoop newGeneration previousGeneration =
        if ((newGeneration |> Array.length) >= maximumMinds) then
          sprintf "New Generation %A" newGeneration |> infoLog
          newGeneration
        else
          let beingId,being = previousGeneration |> Array.head
          let updatedPreviousGeneration =
            let tailGeneration = previousGeneration |> Array.tail
            Array.append tailGeneration [|(beingId, being)|]
          let mutatedBeing : NodeRecords = being |> mutationFunction
          let newId = newGeneration |> Array.length
          let updatedNewGeneration = Array.append newGeneration [|(newId,mutatedBeing)|]
          processEvolutionLoop updatedNewGeneration updatedPreviousGeneration
      processEvolutionLoop Array.empty currentGen
    //TODO optimize this
    generationRecords
    |> Map.toArray
    |> processEvolution
    |> Map.ofArray
  let rec processGenerations (generationCounter : int) (generationRecords : GenerationRecords) : ScoredNodeRecords =
    let scoredGenerationRecords : ScoredNodeRecords =
      let createScoreKeeper (nodeRecordsId, nodeRecords) =
        let scoreKeeper =
          ScoreKeeperInstance.Start(fun inbox ->
            let rec loop outputBuffer =
              async {
                let! someMsg = inbox.TryReceive 250
                match someMsg with
                | None ->
                  return! loop outputBuffer
                | Some msg ->
                  match msg with
                  | Gather (replyChannel, outputHookId, actuatorOutput) ->
                    let updatedBuffer =
                      outputBuffer
                      |> Map.add outputHookId actuatorOutput
                    replyChannel.Reply()
                    return! loop updatedBuffer
                  | GetScore replyChannel ->
                    sprintf "Sending Buffer to fitnessfunction %A" outputBuffer |> infoLog
                    outputBuffer
                    |> fitnessFunction nodeRecordsId
                    |> replyChannel.Reply
                    return! loop Map.empty
                  | KillScoreKeeper replyChannel ->
                    replyChannel.Reply ()
                    ()
              }
            loop Map.empty
          )
          |> (fun x -> x.Error.Add(fun x -> sprintf "%A" x |> infoLog); x)
        (nodeRecordsId, scoreKeeper, nodeRecords)
      let createLiveMind (nodeRecordsId, (scoreKeeper : ScoreKeeperInstance), nodeRecords) =
        let outputHooks : OutputHookFunctions =
          let scoringFunction outputHookId =
            (fun actuatorOutput ->
              (fun r -> Gather (r, outputHookId, actuatorOutput))
              |> scoreKeeper.PostAndReply
            )
          outputHookFunctionIds
          |> Seq.map(fun id -> (id, id |> scoringFunction) )
          |> Map.ofSeq
        let syncFunctions =
          let neededSyncFunctionIds =
            let sensorRecords =
              nodeRecords
              |> Map.filter (fun _ record -> record.NodeType = NodeRecordType.Sensor)
            let getSyncFunctionId (sensorId, sensorRecord : NodeRecord) =
              if (sensorRecord.SyncFunctionId.IsNone) then
                raise(SensorRecordDoesNotHaveASyncFunctionException <| sprintf "Sensor Record %A" sensorRecord.NodeId)
              else
                sensorRecord.SyncFunctionId.Value
            sensorRecords
            |> Map.toSeq
            |> Seq.map getSyncFunctionId

          syncFunctionSources
          |> Map.filter(fun key _ -> neededSyncFunctionIds |> Seq.exists(fun neededId -> key = neededId))
          |> Map.map (fun key syncFunctionSource -> syncFunctionSource nodeRecordsId)
        let cortex =
          {
            ActivationFunctions = activationFunctions
            SyncFunctions = syncFunctions
            OutputHooks = outputHooks
            InfoLog = infoLog
            NodeRecords = nodeRecords
          } |> constructNeuralNetwork
          |> createCortex infoLog
        (nodeRecordsId,scoreKeeper,cortex)
      let processThinkCycles (liveRecordsWithScoreKeepers : (NodeRecordsId*ScoreKeeperInstance*CortexInstance) array) : ScoredNodeRecords =
        let rec scoreThinkCycles scoredThinkCycles =
          if (scoredThinkCycles |> Array.length) >= maximumThinkCycles then
            scoredThinkCycles
          else
            let scoreGenerationThinkCycle =
              let processThink (nodeRecordsId, scoreKeeper, (cortex : CortexInstance)) =
                ThinkAndAct |> cortex.PostAndReply
                nodeRecordsId, scoreKeeper, cortex
              let processScoring =
                let scoreNeuralNetworkThinkCycle (nodeRecordsId, (scoreKeeper : ScoreKeeperInstance), (cortex : CortexInstance)) =
                  let rec waitOnScoreKeeper () =
                    if scoreKeeper.CurrentQueueLength <> 0 then
                      sprintf "Waiting on score keeper to finish gathering results from node records %A" nodeRecordsId
                      |> infoLog
                      System.Threading.Thread.Sleep(200)
                      waitOnScoreKeeper ()
                    else
                      ()
                  waitOnScoreKeeper ()
                  let (score : Score), endOfGenerationOption =
                    GetScore |> scoreKeeper.PostAndReply
                  sprintf "Node Records Id %A scored %A" nodeRecordsId score |> infoLog
                  (nodeRecordsId,score, endOfGenerationOption)
                if (evolutionProperties.AsynchronousScoring) then
                  Array.Parallel.map scoreNeuralNetworkThinkCycle
                else
                  Array.map scoreNeuralNetworkThinkCycle
              liveRecordsWithScoreKeepers
              |> Array.Parallel.map processThink
              |> processScoring
            let updatedScoredThinkCycles =
              let currentThinkCycle =
                scoreGenerationThinkCycle
                |> Array.Parallel.map(fun (nodeRecordsId,score, _) -> nodeRecordsId, score)
              scoredThinkCycles
              |> Array.append currentThinkCycle
            if (scoreGenerationThinkCycle |> Array.exists(fun (_, _, endGenerationOption) -> endGenerationOption = EndGeneration)) then
              updatedScoredThinkCycles
            else
              scoreThinkCycles updatedScoredThinkCycles
        let scoredGeneration =
          let sumScoreOfMind (nodeRecordsId, (scoreArray : ('a*Score) array)) =
            let score =
              scoreArray
              |> Array.sumBy(fun (_, score) -> score)
            nodeRecordsId, score
          //This has to stay synchronous
          //To maintain a steady clock of think cycles for all living minds
          scoreThinkCycles Array.empty
          |> Array.groupBy(fun (nodeRecordsId, score) -> nodeRecordsId)
          |> Array.Parallel.map sumScoreOfMind
          |> Map.ofArray

        let terminateExistenceAndCollectScore (nodeRecordsId, (scoreKeeper : ScoreKeeperInstance), (cortex : CortexInstance)) =
          let updatedRecords = KillCortex |> cortex.PostAndReply
          KillScoreKeeper |> scoreKeeper.PostAndReply
          let score = scoredGeneration |> Map.find nodeRecordsId
          (nodeRecordsId, (score, updatedRecords))

        liveRecordsWithScoreKeepers
        |> Array.Parallel.map terminateExistenceAndCollectScore

      generationRecords
      |> Map.toArray
      |> Array.Parallel.map createScoreKeeper
      |> Array.Parallel.map createLiveMind
      |> processThinkCycles
    let divdeThePopulation (scoredRecords : ScoredNodeRecords) : ScoredNodeRecords =
      let dividedLength =
        let length = (scoredRecords |> Array.length) / evolutionProperties.DividePopulationBy
        if (length < 2) then
          2
        else
          length
      scoredRecords
      |> Array.sortByDescending(fun (_,(score,_)) -> score)
      |> Array.chunkBySize dividedLength
      |> Array.head
    let convertToGenerationRecords (scoredNodeRecords : ScoredNodeRecords) : GenerationRecords =
      scoredNodeRecords
      |> Array.Parallel.map (fun (nodeId, (score,nodeRecord)) -> (nodeId, nodeRecord))
      |> Map.ofArray

    scoredGenerationRecords |> endOfGenerationFunction

    if (generationCounter >= generations) then
      let printScores (scoredRecords : ScoredNodeRecords) : ScoredNodeRecords =
        let rec printScores remainingScoredRecords =
          if remainingScoredRecords |> Array.isEmpty then
            ()
          else
            let key, (score,_) = remainingScoredRecords |> Array.head
            printfn "Neuron Records Id %A : %f" key score
            printScores (remainingScoredRecords |> Array.tail)
        printfn "Scored Generations"
        printfn "-------------------------------"
        scoredRecords
        |> printScores
        scoredRecords

      scoredGenerationRecords
      |> printScores
    else
      scoredGenerationRecords
      |> divdeThePopulation
      |> convertToGenerationRecords
      |> evolveGeneration
      |> processGenerations (generationCounter + 1)
  evolutionProperties.StartingRecords
  |> evolveGeneration
  |> processGenerations 0

let getDefaultTrainingProperties
  (trainingSet : TrainingAnswerAndDataSet<'T>)
    (interpretActuatorOutputFunction : InterpretActuatorOutputFunction<'T>)
      (scoreNeuralNetworkAnswerFunction : ScoreNeuralNetworkAnswerFunction<'T>)
        (activationFunctions : ActivationFunctions)
          (outputHookFunctionIds : OutputHookFunctionIds)
            (learningAlgorithm : NeuronLearningAlgorithm)
              (infoLog : InfoLogFunction)
              : TrainingProperties<'T> =
  let startingGenerationRecords : GenerationRecords =
    let startingRecordId = 0
    let startingNodeRecords = getDefaultNodeRecords activationFunctions outputHookFunctionIds 0 learningAlgorithm infoLog
    Map.empty
    |> Map.add startingRecordId startingNodeRecords

  {
    AmountOfGenerations = defaultEvolutionProperties.Generations
    MaximumThinkCycles = defaultEvolutionProperties.MaximumThinkCycles
    MaximumMinds = defaultEvolutionProperties.MaximumMinds
    ActivationFunctions = activationFunctions
    OutputHookFunctionIds = outputHookFunctionIds
    EndOfGenerationFunctionOption = None
    StartingRecords = startingGenerationRecords
    //TODO Change this to default mutationSequence
    MutationSequence = minimalMutationSequence
    TrainingAnswerAndDataSet = trainingSet
    InterpretActuatorOutputFunction = interpretActuatorOutputFunction
    ScoreNeuralNetworkAnswerFunction = scoreNeuralNetworkAnswerFunction
    ShuffleDataSet = false
    NeuronLearningAlgorithm = learningAlgorithm
    DividePopulationBy = 2
    InfoLog = infoLog
  }

let trainSingleScopeProblem (trainingProperties : TrainingProperties<'T>) =
  let maybeRandom = if (trainingProperties.ShuffleDataSet) then Some(System.Random()) else None
  let getDataGenerator (initialDataSet : TrainingAnswerAndDataSet<'T>) =
    DataGeneratorInstance.Start(fun inbox ->
      let rec loop buffer =
        async {
          let! someMsg = inbox.TryReceive 250
          match someMsg with
          | None -> return! loop buffer
          | Some msg ->
            match msg with
            | GetData (replyChannel, nodeRecordsId) ->
              let updatedBuffer =
                let dataSet =
                  match buffer |> Map.containsKey nodeRecordsId with
                  | true ->
                    buffer |> Map.find nodeRecordsId |> snd
                  | false ->
                    initialDataSet
                let expectedResult, data =
                  match trainingProperties.ShuffleDataSet with
                  | true ->
                    let randomNumber =
                      let random =
                        match maybeRandom with
                        | Some x -> x
                        | None -> raise <| ShuffleDataRandomOptionIsNoneException "Data Generator attempted to shuffle data but random is not accessible"
                      dataSet |> Array.length |> random.Next
                    match dataSet |> Array.tryItem randomNumber with
                    | Some dataTuple -> dataTuple
                    | None -> raise <| DataSetDoesNotHaveIndexException randomNumber
                  | false -> dataSet |> Array.head
                data |> replyChannel.Reply
                let updatedDataSet =
                  match trainingProperties.ShuffleDataSet with
                  | true -> dataSet
                  | false -> Array.append (dataSet |> Array.tail) [|(expectedResult, data)|]
                buffer
                |> Map.add nodeRecordsId (expectedResult, updatedDataSet)
              return! loop updatedBuffer
            | GetExpectedResult (replyChannel, nodeRecordsId) ->
              let expectedResult =
                match buffer |> Map.containsKey nodeRecordsId with
                | true ->
                  buffer |> Map.find nodeRecordsId |> fst
                | false ->
                  initialDataSet |> Array.head |> fst
              expectedResult |> replyChannel.Reply
              return! loop buffer
            | ClearBuffer replyChannel ->
              replyChannel.Reply()
              return! loop Map.empty
            | KillDataGenerator ->
              ()
      }
      loop Map.empty
    )
    |> (fun x -> x.Error.Add(fun x -> sprintf "%A" x |> trainingProperties.InfoLog); x)

  let dataGenerator = getDataGenerator trainingProperties.TrainingAnswerAndDataSet

  let syncFunctionSource : SyncFunctionSource =
    (fun nodeRecordsId ->
      let getDataMsg = (fun r -> GetData(r,nodeRecordsId))
      let syncFunction = (fun () -> getDataMsg |> dataGenerator.PostAndReply)
      syncFunction
    )

  let syncFunctionSources =
    let syncFunctionId = 0
    Map.empty |> Map.add syncFunctionId syncFunctionSource

  let endOfGenerationFunction generationRecords =
    ClearBuffer |> dataGenerator.PostAndReply
    match trainingProperties.EndOfGenerationFunctionOption with
    | Some eogFunc -> eogFunc generationRecords
    | None -> ()

  let fitnessFunction neuronRecordsId actuatorOutputs =
    let expectedMsg = (fun r -> GetExpectedResult(r, neuronRecordsId)) |>  dataGenerator.PostAndReply
    actuatorOutputs
    |> trainingProperties.InterpretActuatorOutputFunction
    |> trainingProperties.ScoreNeuralNetworkAnswerFunction expectedMsg
    |> (fun score -> score, ContinueGeneration)

  let evolutionProperties =
    { defaultEvolutionProperties with
        MaximumThinkCycles = trainingProperties.MaximumThinkCycles
        Generations = trainingProperties.AmountOfGenerations
        MaximumMinds = trainingProperties.MaximumMinds
        MutationSequence = trainingProperties.MutationSequence
        FitnessFunction = fitnessFunction
        ActivationFunctions = trainingProperties.ActivationFunctions
        SyncFunctionSources = syncFunctionSources
        OutputHookFunctionIds = trainingProperties.OutputHookFunctionIds
        EndOfGenerationFunctionOption = Some endOfGenerationFunction
        StartingRecords = trainingProperties.StartingRecords
        NeuronLearningAlgorithm = trainingProperties.NeuronLearningAlgorithm
    }
  evolveForXGenerations evolutionProperties

let getLiveEvolutionInstance liveEvolutionProperties =
  let infoLog = liveEvolutionProperties.InfoLog
  let mutationFunction =
    let mutationSequence = liveEvolutionProperties.MutationSequence
    let activationFunctionIds =
      liveEvolutionProperties.ActivationFunctions
      |> Map.toSeq
      |> Seq.map (fun (id,_) -> id)
    let syncFunctionIds =
      liveEvolutionProperties.SyncFunctions
      |> Map.toSeq
      |> Seq.map (fun (id,_) -> id)
    let outputHookFunctionIds =
      liveEvolutionProperties.OutputHookFunctions
      |> Map.toSeq
      |> Seq.map (fun (id,_) -> id)
    let completeMutationProperties (records : NodeRecords) : MutationProperties =
      {
        Mutations = mutationSequence
        ActivationFunctionIds = activationFunctionIds
        SyncFunctionIds = syncFunctionIds
        OutputHookFunctionIds = outputHookFunctionIds
        LearningAlgorithm = liveEvolutionProperties.NeuronLearningAlgorithm
        InfoLog = liveEvolutionProperties.InfoLog
        NodeRecords = records
      }
    (fun records -> records |> completeMutationProperties |> mutateNeuralNetwork )

  let evolveGeneration (generationRecords : GenerationRecords) : GenerationRecords =
    let processEvolution currentGen =
      let rec processEvolutionLoop newGeneration previousGeneration =
        if ((newGeneration |> Array.length) >= liveEvolutionProperties.MaximumMindsPerGeneration) then
          newGeneration
        else
          let nodeRecordsId, nodeRecords = previousGeneration |> Array.head
          let updatedPreviousGeneration =
            let tailGeneration = previousGeneration |> Array.tail
            Array.append tailGeneration [|(nodeRecordsId, nodeRecords)|]
          let mutatedRecords : NodeRecords = nodeRecords |> mutationFunction
          let newId = newGeneration |> Array.length
          let updatedNewGeneration = Array.append newGeneration [|(newId, mutatedRecords)|]
          processEvolutionLoop updatedNewGeneration updatedPreviousGeneration
      processEvolutionLoop Array.empty currentGen
    //TODO optimize this
    generationRecords
    |> Map.toArray
    |> processEvolution
    |> Map.ofArray
  let createNewActiveCortex nodeRecords =
    {
      ActivationFunctions = liveEvolutionProperties.ActivationFunctions
      SyncFunctions = liveEvolutionProperties.SyncFunctions
      OutputHooks = liveEvolutionProperties.OutputHookFunctions
      InfoLog = infoLog
      NodeRecords = nodeRecords
    } |> constructNeuralNetwork
    |> createCortex infoLog

  LiveEvolutionInstance.Start(fun inbox ->
    let rec loop (currentGeneration : GenerationRecords)
                   (activeCortexAndId : NodeRecordsId*CortexInstance)
                     (thinkCycleCounter : int)
                       (scoresBuffer : ActiveCortexBuffer)
                         (scoredGenerationRecords : ScoredNodeRecords) =
      async {
        let! someMsg = inbox.TryReceive 250
        match someMsg with
        | None ->
          return! loop currentGeneration activeCortexAndId thinkCycleCounter scoresBuffer scoredGenerationRecords
        | Some msg ->
          match msg with
          | SynchronizeActiveCortex replyChannel ->
            let nodeRecordsId, activeCortex = activeCortexAndId
            ThinkAndAct |> activeCortex.PostAndReply
            let score, thinkCycleOption =
              liveEvolutionProperties.FitnessFunction nodeRecordsId
            let updatedScoresBuffer =
              scoresBuffer
              |> Array.append [|score|]
            let updatedThinkCycleCounter = thinkCycleCounter + 1
            if thinkCycleOption = EndThinkCycle || (liveEvolutionProperties.MaximumThinkCycles.IsSome && updatedThinkCycleCounter >= liveEvolutionProperties.MaximumThinkCycles.Value) then
              let updatedRecords = KillCortex |> activeCortex.PostAndReply
              let scoreSum = updatedScoresBuffer |> Array.sum
              let updatedScoredGenerationRecords =
                Array.append scoredGenerationRecords [| (nodeRecordsId, (scoreSum, updatedRecords)) |]
              let amountOfScoredRecords =
                updatedScoredGenerationRecords
                |> Array.length

              if amountOfScoredRecords >= liveEvolutionProperties.MaximumMindsPerGeneration then
                // Process end generation, mutate, then create active cortex
                match liveEvolutionProperties.EndOfGenerationFunctionOption with
                | None -> ()
                | Some endOfGenerationFunction ->
                  updatedScoredGenerationRecords
                  |> endOfGenerationFunction
                let newGeneration =
                  updatedScoredGenerationRecords
                  |> liveEvolutionProperties.FitPopulationSelectionFunction
                  |> evolveGeneration
                let starterNodeRecordsId, starterNodeRecords =
                  newGeneration
                  |> Map.toSeq
                  |> Seq.head
                let newActiveCortexAndId =
                  let newActiveCortex = starterNodeRecords |> createNewActiveCortex
                  starterNodeRecordsId, newActiveCortex

                replyChannel.Reply()
                return! loop newGeneration newActiveCortexAndId 0 Array.empty updatedScoredGenerationRecords
              else
                let desiredNodeRecordsId = (nodeRecordsId+1)
                let desiredNodeRecords =
                  currentGeneration
                  |> Map.find desiredNodeRecordsId
                let newActiveCortex =
                  desiredNodeRecords
                  |> createNewActiveCortex
                replyChannel.Reply()
                return! loop currentGeneration (desiredNodeRecordsId, newActiveCortex) 0 Array.empty updatedScoredGenerationRecords
            else
              replyChannel.Reply()
              return! loop currentGeneration activeCortexAndId updatedThinkCycleCounter updatedScoresBuffer scoredGenerationRecords
          | EndEvolution replyChannel ->
            let nodeRecordsId, activeCortex = activeCortexAndId
            let updatedNodeRecords = KillCortex |> activeCortex.PostAndReply
            let updatedScoredGenerationRecords : ScoredNodeRecords =
              let scoreSum = scoresBuffer |> Array.sum
              Array.append scoredGenerationRecords [| (nodeRecordsId, (scoreSum, updatedNodeRecords)) |]
            updatedScoredGenerationRecords
            |> replyChannel.Reply
      }
    let starterRecords =
      liveEvolutionProperties.StarterRecords
      |> evolveGeneration
    let starterNodeRecordsId, nodeRecords =
      starterRecords
      |> Map.toSeq
      |> Seq.head
    let newActiveCortex =
      nodeRecords
      |> createNewActiveCortex
    loop starterRecords (starterNodeRecordsId, newActiveCortex) 0 Array.empty Array.empty
  ) |> (fun x -> x.Error.Add(fun err -> sprintf "%A" err |> infoLog); x)
