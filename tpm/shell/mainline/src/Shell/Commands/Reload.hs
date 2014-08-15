module Shell.Commands.Reload where
import Shell.API

reloadCommand str = ShellCmd str help desc reload
    where help     = "reload all shell plugins"
          desc     = "reload all shell plugins"
          reload x = shellLoadPlugins
