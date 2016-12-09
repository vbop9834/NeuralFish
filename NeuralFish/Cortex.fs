module NeuralFish.Cortex

open NeuralFish.Types
open NeuralFish.Exceptions
open NeuralFish.Core
open NeuralFish.Exporter

let createCortex infoLog liveNeurons : CortexInstance =
  let rec waitOnAcutuators neuralNetworkToWaitOn =
    let checkIfActuatorsAreReady (neuralNetwork : NeuralNetwork) =
      //returns true if active
      let actuatorIsActive (neuron : NeuronInstance) =
        let maybeActuatorStatus =
          CheckActuatorStatus
          |> neuron.TryPostAndReply
        match maybeActuatorStatus with
        | None -> raise <| NeuronInstanceUnavailableException "Cortex - Neuron instance is not available when trying to check actuators"
        | Some actuatorReady -> actuatorReady
      let checkIfNeuronIsBusy (neuron : NeuronInstance) = neuron.CurrentQueueLength <> 0
      neuralNetwork
      |> Map.exists(fun i (_,neuron) -> neuron |> checkIfNeuronIsBusy || not <| actuatorIsActive neuron  )
    if neuralNetworkToWaitOn |> checkIfActuatorsAreReady then
      //200 milliseconds of sleep seems plenty while waiting on the NN
      System.Threading.Thread.Sleep(200)
      waitOnAcutuators neuralNetworkToWaitOn
    else
      ()

  let registerCortex (neuralNetwork : NeuralNetwork) cortex =
    //TODO do this right. Remove the synchronous behavior and map manipulation
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
          | ThinkAndAct replyChannel ->
            "Cortex - Starting think cycle" |> infoLog
            liveNeurons |> synchronizeNN
            //Sleep to give the NN a chance to process initial messages
            //TODO Think of a better way to handle this intermittent startup
            System.Threading.Thread.Sleep(200)
            "Cortex - Waiting on Neural Network to finish" |> infoLog
            liveNeurons |> waitOnAcutuators
            "Cortex - Think cycle finished. Activating Actuators" |> infoLog
            liveNeurons |> activateActuators
            "Cortex - Actuators Activated" |> infoLog
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

