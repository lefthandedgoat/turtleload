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

//very important
System.Net.ServicePointManager.DefaultConnectionLimit <- 10000

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
  <input type="hidden" name="JobId" value="%s">
  <input type="submit" value="Send">
</form>

<form method="POST" action="/start">
  <input type="hidden" name="JobId" value="%s">
  <input type="submit" value="Start">
</form>

<form method="POST" action="/stop">
  <input type="hidden" name="JobId" value="%s">
  <input type="submit" value="Stop">
</form>

</body>
</html>
  """ jobId jobId jobId

//actor stuff
type actor<'t> = MailboxProcessor<'t>

type Command =
  | Process of string

type Worker =
  | Do of Command

type Manager =
  | Do of int * Command
  | Start
  | Stop

type MetaManager =
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
  let responseTime = stopWatch.Elapsed.TotalMilliseconds
  printfn "%A" responseTime

let newWorker manager : actor<Worker> =
  actor.Start(fun self ->
    let rec loop manager =
      async {
        let! msg = self.Receive ()
        match msg with
        | Worker.Do command ->
          match command with
          | Process uri -> doit uri
          return! loop manager
      }
    loop manager
  )

let newManager () : actor<Manager> =
  actor.Start(fun self ->
    let rec loop status =
      async {
        let! msg = self.Receive ()
        match msg with
        | Manager.Do (count, command) ->
          match status with
          | Stopped ->
            printfn "Can't do work when the job is stopped"
            return! loop status
          | Running ->
            let workers = [ 1 .. count ] |> List.map (fun _ -> newWorker self)
            workers |> List.iter (fun worker -> worker.Post(Worker.Do(command)))
            return! loop status
        | Manager.Stop ->
          return! loop Stopped
        | Manager.Start ->
          return! loop Running
      }
    loop Stopped
  )

let getManager managers jobId =
  let maybeManager = managers |> List.tryFind (fun (jobId', _) -> jobId' = jobId)
  match maybeManager with
    | Some (_, manager) -> manager, managers
    | None ->
      let manager = newManager()
      let managers = (jobId, manager) :: managers
      manager, managers

let newMetaManager () : actor<MetaManager> =
  actor.Start(fun self ->
    let rec loop (managers : (Guid * actor<Manager>) list) =
      async {
        let! msg = self.Receive ()
        let uri = "http://localhost:8083"
        match msg with
        | Send(jobId, count) ->
          let manager, managers = getManager managers jobId
          let count = int count
          manager.Post(Do(count, Process uri))
          return! loop managers
        | Stop jobId ->
          let manager, managers = getManager managers jobId
          manager.Post(Manager.Stop)
          return! loop managers
        | Start jobId ->
          let manager, managers = getManager managers jobId
          manager.Post(Manager.Start)
          return! loop managers
      }
    loop []
  )

let metaManager = newMetaManager()

let webPart =
  choose
    [
      path "/" >>= choose [ GET >>= (OK <| html (System.Guid.NewGuid().ToString())) ]
      path "/send" >>= choose [ POST >>= bindToForm send (fun msg -> metaManager.Post(Send(guid msg.JobId, msg.Message)); (OK <| html msg.JobId)) ]
      path "/stop" >>= choose [ POST >>= bindToForm stop (fun msg -> metaManager.Post(Stop(guid msg.JobId)); (OK <| html msg.JobId)) ]
      path "/start" >>= choose [ POST >>= bindToForm start (fun msg -> metaManager.Post(Start(guid msg.JobId)); (OK <| html msg.JobId)) ]
    ]

startWebServer defaultConfig webPart
