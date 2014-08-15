module Shell.Commands.Repeat where
import Shell.API

repeatCommand str = ShellCmd str help desc repeat
    where help     = "repeat the last action"
          desc     = "repeat the last action"
          repeat x = shellRepeat
