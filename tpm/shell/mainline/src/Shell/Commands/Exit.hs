module Shell.Commands.Exit where
import Shell.API

exitCommand str = ShellCmd str help desc exit
    where exit x = shellExit
          help   = "exit the shell"
          desc   = "exit the shell"
