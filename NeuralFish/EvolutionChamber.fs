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

let mutateNeuralNetwork (mutations : MutationSequence)
  (activationFunctionIds : ActivationFunctionId seq)
    (syncFunctionIds : SyncFunctionId seq)
      (outputHookFunctionIds : OutputHookId seq)
        (learningAlgorithm : NeuronLearningAlgorithm)
          (nodeRecords : NodeRecords) =
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
  let rec processMutationSequence pendingMutations nodeRecords =
    sprintf "Pending Mutations %A" pendingMutations |> infoLog
    if (pendingMutations |> Seq.isEmpty) then
      nodeRecords
    else
      let rec mutate mutation =
        sprintf "Mutating using %A" mutation |> infoLog
        let addOutboundConnection (toNode : NodeRecord) fromNode =
          let newOutboundConnections =
            fromNode.OutboundConnections
            |> Map.add (System.Guid.NewGuid()) (toNode.NodeId,1.0)
          { fromNode with OutboundConnections = newOutboundConnections }
        let selectRandomNode (nodeRecords : NodeRecords) =
          let seqOfNodeRecords = nodeRecords |> Map.toSeq
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
            nodeRecords
          else
            selectRandomMutation () |> mutate

        match mutation with
        | MutateActivationFunction ->
          let _,neuronToMutate =
            nodeRecords
            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
            |> selectRandomNode
          let newActivationFunctionId = selectRandomActivationFunctionId ()
          let mutatedNeuron = { neuronToMutate with ActivationFunctionId = Some newActivationFunctionId }
          nodeRecords
          |> Map.add mutatedNeuron.NodeId mutatedNeuron
        | AddBias ->
          let _,neuronToAddBias =
            nodeRecords
            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
            |> selectRandomNode
          let addBiasToNeuronAndSaveToRecords (nodeRecord : NodeRecord) =
            let addRandomBiasToNeuron (neuron : NodeRecord) =
              let bias : Bias = random.NextDouble()
              sprintf "Adding bias %f to neuron %A" bias neuron.NodeId |> infoLog
              { neuron with Bias = Some bias }
            let updatedNodeRecord = nodeRecord |> addRandomBiasToNeuron
            nodeRecords
            |> Map.add updatedNodeRecord.NodeId updatedNodeRecord
          match neuronToAddBias.Bias with
          | Some bias ->
            if (bias = 0.0) then
              neuronToAddBias
              |> addBiasToNeuronAndSaveToRecords
            else
              if totalNumberOfMutations = 1 then
                nodeRecords
              else
                sprintf "Neuron %A already has bias %f" neuronToAddBias.NodeId bias |> infoLog
                mutateRandomly()
          | None ->
            neuronToAddBias
            |> addBiasToNeuronAndSaveToRecords
        | RemoveBias ->
          let _,neuronToRemoveBias =
            nodeRecords
            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
            |> selectRandomNode
          let removeBiasFromNeuronAndSaveRecords neuron =
            let removeBiasFromNeuron neuron =
              { neuron with Bias = None }
            let updatedNeuron = neuron |> removeBiasFromNeuron
            nodeRecords
            |> Map.add updatedNeuron.NodeId updatedNeuron
          match neuronToRemoveBias.Bias with
            | Some bias ->
              if (bias > 0.0) then
                neuronToRemoveBias
                |> removeBiasFromNeuronAndSaveRecords
              else
                if totalNumberOfMutations = 1 then
                  nodeRecords
                else
                  sprintf "Neuron %A already has no bias already" neuronToRemoveBias.NodeId|> infoLog
                  mutateRandomly()
            | None ->
              if totalNumberOfMutations = 1 then
                nodeRecords
              else
                sprintf "Neuron %A already has no bias already" neuronToRemoveBias.NodeId|> infoLog
                mutateRandomly()
        | MutateWeights ->
          let _, neuronToMutateWeights =
            nodeRecords
            |> Map.filter(fun _ x -> x.NodeType <> NodeRecordType.Actuator)
            |> selectRandomNode
          let mutatedNeuron =
            let probabilityOfWeightMutation =
              let totalNumberOfOutboundConnections = neuronToMutateWeights.OutboundConnections |> Map.toSeq |> Seq.length |> float
              1.0/(sqrt totalNumberOfOutboundConnections)
            let newOutboundConnections =
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
              neuronToMutateWeights.OutboundConnections
              |> Map.map calculateProbabilityAndMutateWeight
            { neuronToMutateWeights with OutboundConnections = newOutboundConnections }

          nodeRecords
          |> Map.add mutatedNeuron.NodeId mutatedNeuron
       // | ResetWeights ->
//        | MutateActivationFunction ->
//          let neuronToMutateAF =
//            nodeRecords
//            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
//            |> selectRandomNode
        | Mutation.AddInboundConnection
        | Mutation.AddOutboundConnection ->
          let _,nodeToAddOutboundConnection =
            nodeRecords
            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
            |> selectRandomNode
          let _,nodeToConnectTo =
            nodeRecords
            |> Map.filter(fun _ x -> x.NodeType <> NodeRecordType.Sensor)
            |> selectRandomNode
          let mutatedNode =
            nodeToAddOutboundConnection
            |> addOutboundConnection nodeToConnectTo
          nodeRecords
          |> Map.add mutatedNode.NodeId mutatedNode
        | AddNeuron ->
          let _,inboundNode =
            nodeRecords
            |> Map.filter(fun _ x -> x.NodeType <> NodeRecordType.Actuator)
            |> selectRandomNode
          let _,outboundNode =
            nodeRecords
            |> Map.filter(fun _ x -> x.NodeType <> NodeRecordType.Sensor)
            |> selectRandomNode
          let blankNewNeuronRecord =
            let seqOfNodes =
              nodeRecords
              |> Map.toSeq
            let layer =
              match inboundNode.Layer < outboundNode.Layer with
              | true ->
                  getRandomDoubleBetween inboundNode.Layer outboundNode.Layer
              | false ->
                  getRandomDoubleBetween outboundNode.Layer inboundNode.Layer
            let outboundConnections = Map.empty
            let nodeId =
              seqOfNodes
              |> Seq.maxBy(fun (nodeId,_) -> nodeId)
              |> (fun (nodeId,_) -> nodeId + 1)
            let activationFunctionId = selectRandomActivationFunctionId ()

            {
              Layer = layer
              NodeId = nodeId
              NodeType = NodeRecordType.Neuron
              OutboundConnections = outboundConnections
              Bias = None
              ActivationFunctionId = Some activationFunctionId
              SyncFunctionId = None
              OutputHookId = None
              MaximumVectorLength = None
              NeuronLearningAlgorithm = learningAlgorithm
            }
          let newNeuronRecordWithOutbound =
            blankNewNeuronRecord
            |> addOutboundConnection outboundNode
          let inboundNodeWithNewNeuron =
            inboundNode
            |> addOutboundConnection newNeuronRecordWithOutbound
          nodeRecords
          |> Map.add newNeuronRecordWithOutbound.NodeId newNeuronRecordWithOutbound
          |> Map.add inboundNodeWithNewNeuron.NodeId inboundNodeWithNewNeuron
       // | OutSplice ->
       // | InSplice ->
        | AddSensorLink ->
          let sensorRecordsThatCanHaveAnotherOutput =
            let determineSensorEligibility key nodeRecord =
              if (nodeRecord.NodeType <> NodeRecordType.Sensor) then
                false
              else if (nodeRecord.MaximumVectorLength |> Option.isNone) then
                false
              else if (nodeRecord.MaximumVectorLength.Value > (nodeRecord.OutboundConnections |> Map.toSeq |> Seq.length)) then
                true
              else
                false
            nodeRecords
            |> Map.filter determineSensorEligibility
          if (sensorRecordsThatCanHaveAnotherOutput |> Map.isEmpty) then
            mutateRandomly()
          else
            let _, sensorNode =
              sensorRecordsThatCanHaveAnotherOutput
              |> selectRandomNode
            let _,outboundNode =
              nodeRecords
              |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
              |> selectRandomNode

            let sensorNodeWithNewOutbound =
              sensorNode
              |> addOutboundConnection outboundNode
            nodeRecords
            |> Map.add sensorNodeWithNewOutbound.NodeId sensorNodeWithNewOutbound
        | AddActuatorLink ->
          let _,inboundNode =
            nodeRecords
            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
            |> selectRandomNode
          let _, actuatorNode =
            nodeRecords
            |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Actuator)
            |> selectRandomNode
          let nodeWithActuatorOutbound =
            inboundNode
            |> addOutboundConnection actuatorNode
          nodeRecords
          |> Map.add nodeWithActuatorOutbound.NodeId nodeWithActuatorOutbound
       // | RemoveSensorLink ->
       // | RemoveActuatorLink ->
        | AddSensor ->
          let sensorRecords = 
            nodeRecords 
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
              nodeRecords
              |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
              |> selectRandomNode
            let blankSensorRecord =
              let layer = 0.0
              let outboundConnections = Map.empty
              let nodeId =
                nodeRecords
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
                OutboundConnections = outboundConnections
                Bias = None
                ActivationFunctionId = None
                SyncFunctionId = Some syncFunctionId
                OutputHookId = None
                MaximumVectorLength = Some 1
                NeuronLearningAlgorithm = NoLearning
              }
            let newSensorWithOutbound =
              blankSensorRecord
              |> addOutboundConnection outboundNode
            nodeRecords
            |> Map.add newSensorWithOutbound.NodeId newSensorWithOutbound
        | AddActuator ->
          let actuatorRecords =
            nodeRecords 
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
              nodeRecords
              |> Map.filter(fun _ x -> x.NodeType = NodeRecordType.Neuron)
              |> selectRandomNode
            let blankActuatorRecord =
              let seqOfNodes =
                nodeRecords
                |> Map.toSeq
              let layer =
                let maxLayer =
                  seqOfNodes
                  |> Seq.maxBy(fun (_,nodeRecord) -> nodeRecord.Layer)
                  |> (fun (_,record) -> record.Layer)
                maxLayer
                |> round
              let outboundConnections = Map.empty
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
                OutboundConnections = outboundConnections
                Bias = None
                ActivationFunctionId = None
                SyncFunctionId = None
                OutputHookId = Some outputHookId
                MaximumVectorLength = None
                NeuronLearningAlgorithm = NoLearning
              }
            let newInboundWithActuatorOutboundConnection =
              inboundNode
              |> addOutboundConnection blankActuatorRecord
            nodeRecords
            |> Map.add newInboundWithActuatorOutboundConnection.NodeId newInboundWithActuatorOutboundConnection
            |> Map.add blankActuatorRecord.NodeId blankActuatorRecord
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
    FitnessFunction = (fun _ _ -> 0.0)
    ActivationFunctions = Map.empty
    SyncFunctionSources = Map.empty
    OutputHookFunctionIds = Seq.empty
    EndOfGenerationFunctionOption = None
    StartingRecords = Map.empty
    NeuronLearningAlgorithm = Hebbian 0.5
    DividePopulationBy = 2
  }

let evolveForXGenerations (evolutionProperties : EvolutionProperties) 
                   : GenerationRecords =
  let activationFunctions = evolutionProperties.ActivationFunctions
  let syncFunctionSources = evolutionProperties.SyncFunctionSources
  let outputHookFunctionIds = evolutionProperties.OutputHookFunctionIds
  let maximumMinds = evolutionProperties.MaximumMinds
  let maximumThinkCycles = evolutionProperties.MaximumThinkCycles
  let fitnessFunction = evolutionProperties.FitnessFunction
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
    (fun records -> records |> mutateNeuralNetwork mutationSequence activationFunctionIds syncFunctionIds outputHookFunctionIds)

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
          let mutatedBeing : NodeRecords = being |> mutationFunction evolutionProperties.NeuronLearningAlgorithm
          let newId = newGeneration |> Array.length
          let updatedNewGeneration = Array.append newGeneration [|(newId,mutatedBeing)|]  
          processEvolutionLoop updatedNewGeneration updatedPreviousGeneration
      processEvolutionLoop Array.empty currentGen
    //TODO optimize this
    generationRecords
    |> Map.toArray
    |> processEvolution
    |> Map.ofArray
  let rec processGenerations (generationCounter : int) (generationRecords : GenerationRecords) : GenerationRecords =
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
          nodeRecords
          |> constructNeuralNetwork activationFunctions syncFunctions outputHooks
          |> createCortex
        (nodeRecordsId,scoreKeeper,cortex)
      let processThinkCycles (liveRecordsWithScoreKeepers : (NodeRecordsId*ScoreKeeperInstance*CortexInstance) array) : ScoredNodeRecords =
        let scoreGenerationThinkCycle _ =
          let generateAsyncThinkTokens (nodeRecordsId, scoreKeeper, (cortex : CortexInstance)) =
            nodeRecordsId, scoreKeeper, Think |> cortex.PostAndAsyncReply, cortex
          let processAsyncTokenParallel currentArray =
            currentArray
            |> Array.Parallel.map(fun (_, _, token, _ ) -> token)
            |> Async.Parallel
            |> Async.RunSynchronously
            |> ignore

            //return the current array for functional flow
            currentArray
          let scoreNeuralNetworkThinkCycle (nodeRecordsId, (scoreKeeper : ScoreKeeperInstance), asyncToken, (cortex : CortexInstance)) =
            let rec waitOnScoreKeeper () =
              if scoreKeeper.CurrentQueueLength <> 0 then
                sprintf "Waiting on score keeper to finish gathering results from node records %A" nodeRecordsId
                |> infoLog
                System.Threading.Thread.Sleep(200)
                waitOnScoreKeeper ()
              else
                ()
            waitOnScoreKeeper ()
            let score : Score =
              GetScore |> scoreKeeper.PostAndReply
            sprintf "Node Records Id %A scored %A" nodeRecordsId score |> infoLog
            (nodeRecordsId,score)
          liveRecordsWithScoreKeepers
          |> Array.Parallel.map generateAsyncThinkTokens
          |> processAsyncTokenParallel
          |> Array.Parallel.map scoreNeuralNetworkThinkCycle

        let scoredGeneration =
          let sumScoreOfMind (nodeRecordsId, (scoreArray : ('a*Score) array)) =
            let score =
              scoreArray
              |> Array.sumBy(fun (_, score) -> score)
            nodeRecordsId, score
          //This has to stay synchronous
          //To maintain a steady clock of think cycles for all living minds
          [|1..maximumThinkCycles|]
          |> Array.map scoreGenerationThinkCycle 
          |> Array.collect id
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
      |> convertToGenerationRecords 
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
              : TrainingProperties<'T> =
  let startingGenerationRecords : GenerationRecords =
    let addNeuronToMap (neuronId, neuronInstance) =
      Map.add neuronId neuronInstance
    let startingRecordId = 0
    let startingNodeRecords =
      let actuator =
        let layer = 1000.0
        let fakeOutputHook = (fun x -> ())
        let nodeId = 0
        let outputHookId = 
          match outputHookFunctionIds |> Seq.tryHead with
          | Some outputHookId -> outputHookId
          | None -> raise <| ActuatorRecordDoesNotHaveAOutputHookIdException "Attempted to generate default training properties but no output hook ids were passed"
        createActuator nodeId layer fakeOutputHook outputHookId
        |> createNeuronInstance
      let neuron =
        let bias = 0.0
        let nodeId = 1
        let layer = 500.0
        let activationFunctionId, activationFunction = 
          match activationFunctions |> Map.toSeq |> Seq.tryHead with 
          | Some xTuple -> xTuple
          | None ->
            raise <| NeuronDoesNotHaveAnActivationFunction "Attempted to generate default traing properties but no activation functions were passed"
        createNeuron nodeId layer activationFunction activationFunctionId bias NoLearning
        |> createNeuronInstance
      let sensor =
        let nodeId = 2
        let syncFunctionId = 0
        let maximumVectorLength = 1
        createSensor nodeId (fun () -> Seq.empty) syncFunctionId maximumVectorLength
        |> createNeuronInstance
      let weight = 0.0
      sensor |> connectSensorToNode neuron [weight]
      neuron |> connectNodeToActuator actuator

      let neuralNetwork =
        Map.empty
        |> addNeuronToMap actuator
        |> addNeuronToMap neuron
        |> addNeuronToMap sensor
      let nodeRecords =
        neuralNetwork
        |> constructNodeRecords
      neuralNetwork |> killNeuralNetwork
      nodeRecords
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
  }

let evolveFromTrainingSet (trainingProperties : TrainingProperties<'T>) =
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
                  Array.append (dataSet |> Array.tail) [|(expectedResult, data)|]
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
    |> (fun x -> x.Error.Add(fun x -> sprintf "%A" x |> infoLog); x)

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
    }
  evolveForXGenerations evolutionProperties 
