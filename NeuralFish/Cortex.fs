module NeuralFish.Cortex

open NeuralFish.Types
open NeuralFish.Exceptions
open NeuralFish.Core
open NeuralFish.Exporter

let createCortex thinkTimeout infoLog (liveNeurons : LiveNeuralNetwork) : CortexInstance =
  let registerCortex (neuralNetwork : LiveNeuralNetwork) cortex =
    let sendCortexToNodes (neuronInstance : NeuronInstance) =
      (fun r -> RegisterCortex (cortex,r)) |> neuronInstance.PostAndReply
    neuralNetwork
    |> Array.Parallel.iter sendCortexToNodes
    cortex
  let resetNeuralNetwork (neuralNetwork : LiveNeuralNetwork) cortex =
    let resetNeuron (neuronInstance : NeuronInstance) =
      ResetNeuron |> neuronInstance.PostAndReply
    neuralNetwork
    |> Array.Parallel.iter (fun neuron -> neuron |> resetNeuron)
    cortex

  let stopwatch = System.Diagnostics.Stopwatch()
  let sendRecurrentSignals (neuralNetwork : LiveNeuralNetwork) cortex =
    neuralNetwork
    |> Array.Parallel.iter(fun (neuronInstance : NeuronInstance) -> SendRecurrentSignals |> neuronInstance.PostAndReply)
    liveNeurons 
    |> waitOnNeuralNetwork false None
    |> ignore
    stopwatch.Stop()
    cortex

  CortexInstance.Start(fun inbox ->
    let rec loop (liveNeurons : LiveNeuralNetwork) =
      async {
        let! someMsg = inbox.TryReceive 250
        match someMsg with
        | None ->
          return! loop liveNeurons
        | Some msg ->
          match msg with
          | ThinkAndAct replyChannel ->
            "Cortex - Starting think cycle" |> infoLog
            liveNeurons |> synchronizeNN
            //This is still sadly necessary
            //Allows the NN to process initial messages
            //TODO Need to find a better solution
            System.Threading.Thread.Sleep(50)
            "Cortex - Waiting on Neural Network to finish" |> infoLog
            stopwatch.Restart()
            let someStopwatch = Some (thinkTimeout,stopwatch)
            let shouldActivateActuators = liveNeurons |> waitOnNeuralNetwork true someStopwatch
            stopwatch.Stop()
            if shouldActivateActuators then
              "Cortex - Think cycle finished. Activating Actuators" |> infoLog
              liveNeurons |> activateActuators
              "Cortex - Actuators Activated" |> infoLog
              ThinkCycleFinished |> replyChannel.Reply
            else
              resetNeuralNetwork liveNeurons inbox |> ignore
              ThinkCycleIncomplete |> replyChannel.Reply
            return! loop liveNeurons
          | KillCortex replyChannel ->
            "Cortex - Gathering Updated Node Records" |> infoLog
            let updatedNodeRecords = liveNeurons |> constructNodeRecords
            "Cortex - Killing Neural Network" |> infoLog
            liveNeurons |> killNeuralNetwork
            "Cortex - Replying with Updated records" |> infoLog
            updatedNodeRecords |> replyChannel.Reply
            ()
      }
    loop liveNeurons
  )
  |> registerCortex liveNeurons
  |> resetNeuralNetwork liveNeurons
  |> sendRecurrentSignals liveNeurons
  |> (fun x -> x.Error.Add(fun x -> sprintf "%A" x |> infoLog); x)
