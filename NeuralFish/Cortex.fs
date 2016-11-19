module NeuralFish.Cortex

open NeuralFish.Types
open NeuralFish.Exceptions
open NeuralFish.Core
open NeuralFish.Exporter

let createCortex liveNeurons : CortexInstance =
  let rec waitOnAcutuators neuralNetworkToWaitOn =
    let checkIfActuatorsAreReady (neuralNetwork : NeuralNetwork) =
      //returns true if active
      let actuatorIsActive (neuron : NeuronInstance) =
        let maybeActuatorStatus =
          CheckActuatorStatus
          |> neuron.TryPostAndReply
        match maybeActuatorStatus with
        | None -> raise <| NeuronInstanceUnavailableException "Cortex - Neuron instance is not available when trying to check actuators"
        | Some actuatorReady ->
          match actuatorReady with
          | true ->
            true
          | false ->
            "Cortex - Actuator is not ready... Waiting" |> infoLog
            System.Threading.Thread.Sleep(200)
            false
      let checkIfNeuronIsBusy (neuron : NeuronInstance) =
        if neuron.CurrentQueueLength <> 0 then
          "Cortex - Neuron is busy... Waiting" |> infoLog
          System.Threading.Thread.Sleep(200)
          true
        else
          false
      neuralNetwork
      |> Map.exists(fun i (_,neuron) -> neuron |> checkIfNeuronIsBusy && not <| actuatorIsActive neuron  )
    if neuralNetworkToWaitOn |> checkIfActuatorsAreReady then
      //200 milliseconds of sleep seems plenty while waiting on the NN
      System.Threading.Thread.Sleep(200)
      waitOnAcutuators neuralNetworkToWaitOn
    else
      ()

  let registerCortex (neuralNetwork : NeuralNetwork) cortex =
    let sendCortexToActuatorAsync _ (_, neuronInstance : NeuronInstance) : Async<unit> =
      (fun r -> RegisterCortex (cortex,r)) |> neuronInstance.PostAndAsyncReply
    neuralNetwork
    |> Map.map sendCortexToActuatorAsync
    |> Map.iter (fun _ asyncthingy -> asyncthingy |> Async.RunSynchronously)

    cortex

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
            liveNeurons |> waitOnAcutuators
            liveNeurons |> activateActuators
            "Cortex - Think cycle finished" |> infoLog
            replyChannel.Reply ()
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
  |> (fun x -> x.Error.Add(fun x -> sprintf "%A" x |> infoLog); x)

