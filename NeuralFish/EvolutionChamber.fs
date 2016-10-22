module NeuralFish.EvolutionChamber

open NeuralFish.Types
open NeuralFish.Core
open NeuralFish.Exporter

type Mutation =
  | AddBias
  | RemoveBias
  // | MutateWeights
  // //Choose random neuron, perturb each weight with probability of
  // //1/sqrt(# of weights)
  // //Intensity chosen randomly between -pi/2 and pi/2
  // | ResetWeights
  // //Choose random neuron AddBias, reset all weights to random values
  // // ranging between -pi/2 and pi/2
  // | MutateActivationFunction
  // //Choose a random neuron A, change activation function to random activation function
  // | AddInboundConnection
  // //Choose a random neuron A, node B, and add a connection
  // | AddOutboundConnection
  // | AddNeuron
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

let mutateNeuralNetwork (mutations : MutationSequence) (nodeRecords : NodeRecords)  =
  let random = System.Random()
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
        let selectRandomNode (nodeRecords : NodeRecords) =
          let seqOfNodeRecords = nodeRecords |> Map.toSeq
          let randomNumber =
            seqOfNodeRecords
            |> Seq.length
            |> random.Next
          seqOfNodeRecords
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
          // | MutateWeights ->
          // | ResetWeights ->
          // | MutateActivationFunction ->
          // | AddInboundConnection ->
          // | AddOutboundConnection ->
          // | AddNeuron ->
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
