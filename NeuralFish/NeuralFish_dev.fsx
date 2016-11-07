#I __SOURCE_DIRECTORY__
#I "../NeuralFish.Tests"

#load "Types.fs"
#load "NeuralFish.fs"
#load "Exporter.fs"
#load "EvolutionChamber.fs"
#load "TestHelper.fs"

open NeuralFish.Types
open NeuralFish.Exporter
open NeuralFish.EvolutionChamber
open NeuralFish.Core
open NeuralFish.Tests.TestHelper

let testHook x = printfn "%f" x
