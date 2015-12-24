open System
open Suave
open Suave.Form
open Suave.Http.Successful
open Suave.Web
open Suave.Http
open Suave.Http.Applicatives
open Suave.Model.Binding
open Suave.Http.ServerErrors
open HttpFs.Client

//helpers
let guid guid = System.Guid.Parse(guid)

//web stuff
let logAndShow500 error =
  printfn "%A" error
  INTERNAL_ERROR "ERROR"

let bindToForm form handler =
  bindReq (bindForm form) handler logAndShow500

type Send = { JobId : string; Message : string; }
type Stop = { JobId : string; }
type Start = { JobId : string; }

let send : Form<Send> = Form ([],[])
let stop : Form<Stop> = Form ([],[])
let start : Form<Start> = Form ([],[])

let html jobId =
 sprintf """
<html>
<body>

<form method="POST" action="/send">
  Message:
  <input type="text" name="Message">
  <input type="hidden" name="JobId" value="%A">
  <input type="submit" value="Send">
</form>

<form method="POST" action="/start">
  <input type="hidden" name="JobId" value="%A">
  <input type="submit" value="Start">
</form>

<form method="POST" action="/stop">
  <input type="hidden" name="JobId" value="%A">
  <input type="submit" value="Stop">
</form>

</body>
</html>
  """ jobId jobId jobId

//actor stuff
type actor<'t> = MailboxProcessor<'t>

type Command =
  | Process of string

type Manager =
  | Do of Command
  | Start
  | Stop

type JobManager =
  | Send of jobId : Guid * message : string
  | Stop of jobId : Guid
  | Start of jobId : Guid

type Status =
  | Running
  | Stopped

let doit uri =
  let stopWatch = System.Diagnostics.Stopwatch.StartNew()
  let request = createRequest Get <| Uri(uri)
  use response = getResponse request |> Async.RunSynchronously
  let body = Response.readBodyAsString response |> Async.RunSynchronously
  let responseTime = stopWatch.Elapsed.TotalMilliseconds
  printfn "%A" responseTime
  printfn "%A" response
  printfn "%A" body

let newManager () : actor<Manager> =
  actor.Start(fun inbox ->
    let rec loop status =
      async {
        let! msg = inbox.Receive ()
        match msg with
        | Do command ->
          match status with
          | Stopped ->
            printfn "Can't do work when the job is stopped"
            return! loop status
          | Running ->
            match command with
            | Process uri -> doit uri
            return! loop status
        | Manager.Stop ->
          return! loop Stopped
        | Manager.Start ->
          return! loop Running
      }
    loop Stopped
  )

let newJobManager () : actor<JobManager> =
  actor.Start(fun inbox ->
    let rec loop (managers : (Guid * actor<Manager>) list) =
      async {
        let! msg = inbox.Receive ()
        let uri = "http://localhost:8083"
        match msg with
        | Send(jobId, _) ->
          let maybeManager = managers |> List.tryFind (fun (jobId', _) -> jobId' = jobId)
          match maybeManager with
          | Some(_, manager) ->
            manager.Post(Do(Process uri))
            return! loop managers
          | None ->
            let manager = newManager()
            manager.Post(Do(Process uri))
            let managers = (jobId, manager) :: managers
            return! loop managers
        | Stop jobId ->
          let _, manager = managers |> List.find (fun (jobId', _) -> jobId' = jobId)
          manager.Post(Manager.Stop)
          return! loop managers
        | Start jobId ->
          let _, manager = managers |> List.find (fun (jobId', _) -> jobId' = jobId)
          manager.Post(Manager.Start)
          return! loop managers
      }
    loop []
  )

let jobManager = newJobManager()

let webPart =
  choose
    [
      path "/" >>= choose [ GET >>= (OK <| html (System.Guid.NewGuid())) ]
      path "/send" >>= choose [ POST >>= bindToForm send (fun msg -> jobManager.Post(Send(guid msg.JobId, msg.Message)); OK "Sent") ]
      path "/stop" >>= choose [ POST >>= bindToForm stop (fun msg -> jobManager.Post(Stop(guid msg.JobId)); OK "Stopped") ]
      path "/start" >>= choose [ POST >>= bindToForm start (fun msg -> jobManager.Post(Start(guid msg.JobId)); OK "Started") ]
    ]

startWebServer defaultConfig webPart
