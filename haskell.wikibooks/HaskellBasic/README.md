# HaskellBasic


host@hskuser:~/dev/vscode.wks/haskell.learning/haskell.wikibooks$ stack new HaskellBasic new-template
Downloading template "new-template" to create project "HaskellBasic" in HaskellBasic/ ...

The following parameters were needed by the template but not provided: author-name
You can provide them in /home/corrado/.stack/config.yaml, like this:
templates:
  params:
    author-name: value
Or you can pass each one as parameters like this:
stack new HaskellBasic new-template -p "author-name:value"


The following parameters were needed by the template but not provided: author-email, author-name, category, copyright, github-username
You can provide them in /home/corrado/.stack/config.yaml, like this:
templates:
  params:
    author-email: value
    author-name: value
    category: value
    copyright: value
    github-username: value
Or you can pass each one as parameters like this:
stack new HaskellBasic new-template -p "author-email:value" -p "author-name:value" -p "category:value" -p "copyright:value" -p "github-username:value"

Looking for .cabal or package.yaml files to use to init the project.
Using cabal packages:
- HaskellBasic/

Selecting the best among 15 snapshots...

Downloaded lts-13.18 build plan.    
Didn't see Chart-1.9.1@sha256:cc50e1e65738ab2d6c30ae9e3f138962f87667d81f7860a0deeca9053f6d21ed,2857 in your package indices.
Updating and trying again.
Selected mirror https://s3.amazonaws.com/hackage.fpcomplete.com/                                 
Downloading timestamp                                                                            
Downloading snapshot                                                                             
Updating index                                                                                   
Updated package index downloaded                                                                 
Update complete                                                                                  
Populated index cache.    
* Matches lts-13.18

Selected resolver: lts-13.18
Initialising configuration using resolver: lts-13.18
Total number of user packages considered: 1
Writing configuration to file: HaskellBasic/stack.yaml
All done.



export LC_ALL=en_US.UTF-8
ghci -isrc:app app/Main.hs
Prelude> :set prompt "\x03BB:"
