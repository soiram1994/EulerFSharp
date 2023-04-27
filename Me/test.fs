namespace test

open System
open System.Collections.Generic
open System.Linq
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging

type Test(logger: ILogger<Test>) = 
    inherit BackgroundService()

    override _.ExecuteAsync(ct: CancellationToken) = 
        async {
            while not ct.IsCancellationRequested do 
                logger.LogInformation "This is copy pasted"
                do! Async.Sleep(1000)
        }
        |> Async.StartAsTask
        :> Task