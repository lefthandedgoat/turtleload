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

type SendFormData =
  {
    JobId : string
    NumberOfRequests : string
    MaxWorkers : string
  }

type SendData =
  {
    JobId : System.Guid
    NumberOfRequests : int
    MaxWorkers : int
  }

let convertSendData (sendFormData : SendFormData) =
  {
    JobId = guid sendFormData.JobId
    NumberOfRequests = int sendFormData.NumberOfRequests
    MaxWorkers = int sendFormData.MaxWorkers
  }

let send : Form<SendFormData> = Form ([],[])

let html jobId =
 sprintf """
<html>
<body>

<form method="POST" action="/send">
  Number of requests:
  <input type="text" name="NumberOfRequests">
  <br/>
  Max number of concurrent requests:
  <input type="text" name="MaxWorkers">
  <input type="hidden" name="JobId" value="%s">
  <input type="submit" value="Send">
</form>

</body>
</html>
  """ jobId

//actor stuff
type actor<'t> = MailboxProcessor<'t>

type Command =
  | Process of string

type Worker =
  | Do
  | Retire

type Manager =
  | Initialize of SendData * Command
  | WorkerDone

type MetaManager =
  | Send of SendData

let doit uri =
  let stopWatch = System.Diagnostics.Stopwatch.StartNew()
  let request = createRequest Get <| Uri(uri)
  use response = getResponse request |> Async.RunSynchronously
  let responseTime = stopWatch.Elapsed.TotalMilliseconds
  sprintf "%A" responseTime

let newWorker id (manager : actor<Manager>) (command : Command): actor<Worker> =
  actor.Start(fun self ->
    let rec loop () =
      async {
        let! msg = self.Receive ()
        match msg with
        | Worker.Retire ->
          manager.Post(Manager.WorkerDone)
          return ()
        | Worker.Do ->
          match command with
          | Process uri ->
            doit uri |> ignore
          return! loop ()
      }
    loop ())

let rec haveWorkerWork (workers : actor<Worker> list) maxWorkers activeWorkers =
  if activeWorkers < maxWorkers then
    match workers with
    | worker :: remainingWorkers ->
      worker.Post(Worker.Do)
      worker.Post(Retire)
      //recurse in case there arent enough working workers
      haveWorkerWork remainingWorkers maxWorkers (activeWorkers + 1)
    | [] -> [], activeWorkers
  else //nothing changes until a worker frees up
    workers, activeWorkers

let newManager () : actor<Manager> =
  actor.Start(fun self ->
    let rec loop (workers : actor<Worker> list) maxWorkers activeWorkers =
      async {
        let! msg = self.Receive ()
        match msg with
        | Manager.Initialize (sendData, command) ->
          //build up a list of all the work to do
          let workers = [ 1 .. sendData.NumberOfRequests ] |> List.map (fun id -> newWorker id self command)
          let maxWorkers = sendData.MaxWorkers
          let activeWorkers = 0
          let remainingWorkers, activeWorkers = haveWorkerWork workers maxWorkers activeWorkers
          return! loop remainingWorkers maxWorkers activeWorkers
        | Manager.WorkerDone ->
          let remainingWorkers, activeWorkers = haveWorkerWork workers maxWorkers (activeWorkers - 1)
          return! loop remainingWorkers maxWorkers activeWorkers
      }
    loop [] 0 0)

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
        let uri = "http://localhost:3000/"
        match msg with
        | Send sendData ->
          let manager, managers = getManager managers sendData.JobId
          manager.Post(Initialize(sendData, Process uri))
          return! loop managers
      }
    loop [])

let metaManager = newMetaManager()

let webPart =
  choose
    [
      path "/" >>= choose [ GET >>= (OK <| html (System.Guid.NewGuid().ToString())) ]
      path "/test" >>= choose [ GET >>= OK "this is a test" ]
      path "/send" >>= choose [ POST >>= bindToForm send
                                           (fun sendFormData ->
                                              metaManager.Post(Send(convertSendData sendFormData))
                                              OK <| html sendFormData.JobId) ]
    ]

startWebServer defaultConfig webPart
