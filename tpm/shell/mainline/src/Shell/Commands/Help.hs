module Shell.Commands.Help where
import Shell.API

helpCommand str = ShellCmd str helpst desc help
    where help [] = showHelp
          help c  = showDesc c
          helpst  = "show this help message"
          desc    = "show this help message"
