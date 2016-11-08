module NeuralFish.Cortex

open NeuralFish.Types
open NeuralFish.Core
open NeuralFish.Exporter
open NeuralFish.EvolutionChamber

let createCortex liveNeurons : CortexInstance =
  let rec waitOnNeuralNetwork neuralNetworkToWaitOn =
    let checkIfNeuralNetworkIsActive (neuralNetwork : NeuralNetwork) =
      //returns true if active
      neuralNetwork
      |> Map.forall(fun i (_,neuron) -> neuron.CurrentQueueLength <> 0)
    if neuralNetworkToWaitOn |> checkIfNeuralNetworkIsActive then
      //200 milliseconds of sleep seems plenty while waiting on the NN
      System.Threading.Thread.Sleep(200)
      waitOnNeuralNetwork neuralNetworkToWaitOn
    else
    ()

  CortexInstance.Start(fun inbox ->
    let rec loop liveNeurons = 
      async {
        let! someMsg = inbox.TryReceive 250
        match someMsg with
        | None ->
          return! loop liveNeurons 
        | Some msg ->
          match msg with
          | Think replyChannel ->
            "Cortex - Starting think cycle" |> infoLog
            liveNeurons |> synchronizeNN
            liveNeurons |> waitOnNeuralNetwork
            "Cortex - Think cycle finished" |> infoLog
            replyChannel.Reply ()
            return! loop liveNeurons
          | KillCortex replyChannel ->
            "Cortex - Killing Neural Network" |> infoLog
            let updatedNodeRecords = liveNeurons |> constructNodeRecords
            liveNeurons |> killNeuralNetwork
            updatedNodeRecords |> replyChannel.Reply
            () 
      }
    loop liveNeurons 
  )

