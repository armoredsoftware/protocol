greetings, instructions on how to use this script to its fullest:

Motivation: 
It would be very useful to start a text editor from a 
terminal and still be able to use that terminal. i.e.
detatch the terminal from the process of text editing.


Instructions:
1. place the file "openinbackground" in your bin directory
2. Add the following to your .bashrc file:
      alias emacs='openinbackground emacs'
      alias gedit='openinbackground gedit'
      ...
      ...
3. That's it! (you may need to source .bashrc to use first)

NOTES:
 Emacs and other text editors allow you to open multiple documents
 simultaneously by passing them as arguements. For instance typing:
 emacs textfile1.txt textfile2.txt will open both files in emacs.
 At this time the 'openinbackground' script will only work if ONE
 text file is passed. For example, given that the above emacs alias
 is in place, typing 'emacs textfile1.txt textfile2.txt' will simply
 open textfile1.txt in the background, discarding the second argument.
 However multiple files would be super easy to implement.
