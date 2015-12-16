open Suave
open Suave.Form
open Suave.Http.Successful
open Suave.Web
open Suave.Http
open Suave.Http.Applicatives
open Suave.Model.Binding
open Suave.Http.ServerErrors

let logAndShow500 error =
  printfn "%A" error
  INTERNAL_ERROR "ERROR"

let bindToForm form handler =
  bindReq (bindForm form) handler logAndShow500

type Message = {
  Message : string;
}

let message : Form<Message> = Form ([],[])

type actor<'t> = MailboxProcessor<'t>

type Command =
  | Say of string

type Worker =
  | Printer of int
  | Yeller of int

type Manager =
  | Add of Worker
  | Do of Command

let getManager () : actor<Manager> =
  actor.Start(fun inbox ->
    let rec loop workers =
      async {
        let! msg = inbox.Receive ()
        match msg with
        | Add worker ->
          let workers = workers @ [worker]
          return! loop workers
        | Do command ->
          let worker = workers |> List.head
          let others = workers |> List.tail
          match command with
            | Say(message) ->
              match worker with
                | Printer(id) -> printfn "I am printer %i, my message is %s" id message
                | Yeller(id) -> printfn "I AM YELLER %i, my message is %s" id message
          let workers = others @ [worker]
          return! loop workers
      }
    loop []
  )


let manager = getManager()
manager.Post(Add(Printer 1))
manager.Post(Add(Yeller 2))
manager.Post(Add(Printer 3))
manager.Post(Add(Printer 4))

manager.Post(Do(Say "Hello"))
manager.Post(Do(Say "World"))

let html =
  """
<html>
<body>

<form method="POST" action="/">
  Message:
  <input type="text" name="Message">
  <input type="submit" value="Submit">
</form>

</body>
</html>
  """

let webPart =
  choose
    [
      path "/" >>=
        choose
          [
            GET >>= OK html
            POST >>= bindToForm message (fun msg ->
              manager.Post(Do(Say msg.Message))
              OK "Done")
          ]
    ]

startWebServer defaultConfig webPart
