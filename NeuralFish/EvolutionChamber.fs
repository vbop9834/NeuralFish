module NeuralFish.EvolutionChamber

open NeuralFish.Types
open NeuralFish.Core
open NeuralFish.Exporter
open NeuralFish.Cortex

type Mutation =
  | MutateActivationFunction
  | AddBias
  | RemoveBias
  | MutateWeights
  // //Choose random neuron, perturb each weight with probability of
  // //1/sqrt(# of weights)
  // //Intensity chosen randomly between -pi/2 and pi/2
  // | ResetWeights
  // //Choose random neuron, reset all weights to random values
  // // ranging between -pi/2 and pi/2
  | AddInboundConnection
  // //Choose a random neuron A, node B, and add a connection
  | AddOutboundConnection
  | AddNeuron
  // //Create a new neuron A, position randomly in NN.
  // //Random Activation Function
  // //Random inbound and outbound
  // | OutSplice
  // | InSplice
  // //Create a new neuron A and sandwich between two nodes
  | AddSensorLink
  | AddActuatorLink
  // | RemoveSensorLink
  // | RemoveActuatorLink
  | AddSensor
  | AddActuator
  // | RemoveInboundConnection
  // | RemoveOutboundConnection
  // | RemoveNeuron
  // //Remove neuron then connect inputs to outputs
  // | DespliceOut
  // | DespliceIn
  // | RemoveSensor
  // | RemoveActuator

type MutationSequence = Mutation seq

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
        (nodeRecords : NodeRecords) =

  let random = System.Random()
  let getRandomDoubleBetween minValue maxValue =
    random.NextDouble() * (maxValue - minValue) + minValue
  let totalNumberOfMutations = mutations |> Seq.length
  let selectRandomMutation _ =
    mutations |> Seq.item (totalNumberOfMutations |> random.Next)
  let pendingMutations =
    let numberOfMutations =
      random.NextDouble() * (sqrt (nodeRecords |> Map.toSeq |> Seq.length |> float))
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
            activationFunctionIds
            |> Seq.length
            |> random.Next
          activationFunctionIds
          |> Seq.item randomNumber
        let selectRandomSyncFunctionId () =
          let randomNumber =
            syncFunctionIds
            |> Seq.length
            |> random.Next
          syncFunctionIds
          |> Seq.item randomNumber
        let selectRandomOutputHookFunctionId () =
          let randomNumber =
            outputHookFunctionIds
            |> Seq.length
            |> random.Next
          outputHookFunctionIds
          |> Seq.item randomNumber

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
                selectRandomMutation () |> mutate
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
                  selectRandomMutation () |> mutate
            | None ->
              if totalNumberOfMutations = 1 then
                nodeRecords
              else
                sprintf "Neuron %A already has no bias already" neuronToRemoveBias.NodeId|> infoLog
                selectRandomMutation () |> mutate
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
            selectRandomMutation () |> mutate
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
            let syncFunctionId = selectRandomSyncFunctionId ()

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
            }
          let newSensorWithOutbound =
            blankSensorRecord
            |> addOutboundConnection outboundNode
          nodeRecords
          |> Map.add newSensorWithOutbound.NodeId newSensorWithOutbound
        | AddActuator ->
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
            let outputHookId = selectRandomOutputHookFunctionId ()

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


let evolveForXGenerations (maximumMinds : int)
  (maximumThinkCycles : int)
    (mutationSequence : MutationSequence)
      (fitnessFunction : FitnessFunction)
        (activationFunctions : ActivationFunctions)
          (syncFunctionSources : SyncFunctionSources)
            (outputHookFunctionIds : OutputHookFunctionIds)
              (endOfGenerationFunction : EndOfGenerationFunction)
                (generations : int)
                  (startingRecords : GenerationRecords)
                   : GenerationRecords =
  let mutationFunction =
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
  let rec processGenerations (generationCounter : int) (generationRecords : GenerationRecords) : GenerationRecords =
    let scoredGenerationRecords : ScoredNodeRecords =
      let scoreNeuralNetwork (nodeRecordsId : NodeRecordsId) (nodeRecords : NodeRecords) =
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
            let getSyncFunctionId (sensorId,sensorRecord) =
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
        let thinkAndScore _ =
          Think |> cortex.PostAndReply
          let score =
            GetScore |> scoreKeeper.PostAndReply
          sprintf "Node Records Id %A scored %A" nodeRecordsId score |> infoLog
          score
        let sumScore : Score =
          [0..maximumThinkCycles]
          |> Seq.map thinkAndScore
          |> Seq.sum
        KillScoreKeeper |> scoreKeeper.PostAndReply
        let updatedRecords = KillCortex |> cortex.PostAndReply
        sumScore, updatedRecords
      generationRecords
      |> Map.toArray
      |> Array.Parallel.map(fun (nodeRecordsId, nodeRecords) -> (nodeRecordsId, nodeRecords |> scoreNeuralNetwork nodeRecordsId))
    let halfThePopulation (scoredRecords : ScoredNodeRecords) : ScoredNodeRecords =
      let halfLength =
        let half = (scoredRecords |> Array.length) / 2
        if (half < 2) then
          2
        else
          half
      scoredRecords
      |> Array.sortByDescending(fun (_,(score,_)) -> score)
      |> Array.chunkBySize halfLength
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
      |> halfThePopulation
      |> convertToGenerationRecords 
      |> evolveGeneration
      |> processGenerations (generationCounter + 1)
  startingRecords
  |> evolveGeneration
  |> processGenerations 0
