module NeuralFish.EvolutionChamber

open NeuralFish.Types
open NeuralFish.Core
open NeuralFish.Exporter

type Mutation =
  | AddBias
  | RemoveBias
  | MutateWeights
  // //Choose random neuron, perturb each weight with probability of
  // //1/sqrt(# of weights)
  // //Intensity chosen randomly between -pi/2 and pi/2
  // | ResetWeights
  // //Choose random neuron, reset all weights to random values
  // // ranging between -pi/2 and pi/2
  // | MutateActivationFunction
  // //Choose a random neuron A, change activation function to random activation function
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
  // | AddSensorLink
  // | AddActuatorLink
  // | RemoveSensorLink
  // | RemoveActuatorLink
  // | AddSensor
  // | AddActuator
  // | RemoveInboundConnection
  // | RemoveOutboundConnection
  // | RemoveNeuron
  // //Remove neuron then connect inputs to outputs
  // | DespliceOut
  // | DespliceIn
  // | RemoveSensor
  // | RemoveActuator

type MutationSequence = Mutation seq

let mutateNeuralNetwork (mutations : MutationSequence)
  (activationFunctions : ActivationFunctions)
    (syncFunctions : SyncFunctions)
      (outputHooks : OutputHookFunctions)
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
      |> round
      |> int
    sprintf "Selecting %i number of mutations" numberOfMutations
    |> infoLog

    [0..numberOfMutations]
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
        let activationFunctionIds = activationFunctions |> Map.toSeq |> Seq.map (fun (id, _) -> id)
        let selectRandomActivationFunctionId () =
          let randomNumber =
              activationFunctionIds
              |> Seq.length
              |> random.Next
          activationFunctionIds
          |> Seq.item randomNumber

        match mutation with
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
            |> Map.filter(fun _ x -> x.NodeType <> NodeRecordType.Actuator)
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
              let maxLayer =
                seqOfNodes
                |> Seq.maxBy(fun (_,nodeRecord) -> nodeRecord.Layer)
                |> (fun (_,record) -> record.Layer)
              getRandomDoubleBetween 1.0 maxLayer
              |> floor
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
       // | AddSensorLink ->
       // | AddActuatorLink ->
       // | RemoveSensorLink ->
       // | RemoveActuatorLink ->
       // | AddSensor ->
       // | AddActuator ->
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
