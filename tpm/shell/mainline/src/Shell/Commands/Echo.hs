module Shell.Commands.Echo where
import Shell.API

echoCommand = ShellCmd [] help desc show
    where show x = shellPutStrLn $ x
          help   = "echo to the screen"
          desc   = "echo to the screen"
