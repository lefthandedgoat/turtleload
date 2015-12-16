namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("turtleload")>]
[<assembly: AssemblyProductAttribute("turtleload")>]
[<assembly: AssemblyDescriptionAttribute("A self contained basic load testing tool")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
