
-- | A Simple DSL for describing and generating Dockerfiles in Haskell
--
-- @
-- main :: IO ()
-- main = do
--   let df = dockerfile $ do
--              from "debian:trusty"
--              maintainer "creichert <creichert07@gmail.com>"
--              run "apt-get -y update"
--              run "apt-get -y upgrade"
--              cmd [ "echo", "hello world"]
--   putStrLn df
-- @

{-# LANGUAGE OverloadedStrings #-}

module Data.Docker
       (
         -- * Types
         Docker
       , dockerfile

         -- * Docker Instructions
       , from
       , maintainer
       , run
       , env
       , add
       , expose
       , copy
       , cmd
       , entrypoint
       , user
       , workdir
       , volume
         -- ONBUILD not implemented yet
         --, onbuild
       ) where


import Control.Monad.Writer

type DockerFile = [Instruction]
type Docker a = Writer DockerFile a


dockerfile :: Docker a -> String
dockerfile = unlines . map prettyCmd . execWriter


type Script = String
type ScriptFile = FilePath
type Param = String
type Distro = String


-- | Dockerfile instruction set
data Instruction
  = From Distro
    -- ^ The FROM instruction sets the Base Image for subsequent
    -- instructions.
    --
    -- <https://docs.docker.com/reference/builder/#from>
    --
    -- * FROM must be the first non-comment instruction in the Dockerfile.
    --
    -- * FROM can appear multiple times within a single Dockerfile in order to
    -- create multiple images.
    --
    -- Syntax:
    --
    -- @
    -- FROM <image>
    -- FROM <image>:<tag>
    -- FROM <image>@<digest>
    -- @
    --

  | Maintainer String
    -- ^ The MAINTAINER instruction allows you to set the Author field
    -- of the generated images.

  | Run Script  -- File [ScriptParam]
    -- ^ RUN has 2 forms:
    --
    -- Syntax:
    -- @
    -- RUN <command> (the command is run in a shell - /bin/sh -c - shell form)
    -- RUN ["executable", "param1", "param2"] (exec form)
    -- @

  | Cmd [ ScriptFile ]
    -- ^ The CMD instruction has three forms:
    --
    -- Syntax:
    --
    -- @
    -- CMD ["executable","param1","param2"] (exec form, this is the preferred form)
    -- CMD ["param1","param2"] (as default parameters to ENTRYPOINT)
    -- CMD command param1 param2 (shell form)
    -- @
    --
    -- There can only be one CMD instruction in a Dockerfile. If you list
    -- more than one CMD then only the last CMD will take effect.
    -- @
    --
    -- If the CMD instruction does not specify an executable, an
    -- ENTRYPOINT instruction must be present.

  | Label String (Maybe String)
    -- ^ The LABEL instruction adds metadata to an image. A LABEL is a
    -- key-value pair. To include spaces within a LABEL value, use quotes and
    -- blackslashes as you would in command-line parsing.
    --
    -- Syntax:
    --
    -- @
    -- LABEL com.example.label-without-value
    -- LABEL com.example.label-with-value="foo"
    -- LABEL version="1.0"
    -- LABEL description="This text illustrates \
    -- that label-values can span multiple lines."
    -- @
  | Expose Int
    -- ^ EXPOSE <port> [<port>...]
  | Env String String
    -- ^
    --
    --   The ENV instruction sets the environment variable <key> to the
    -- value <value>. This value will be in the environment of all
    -- "descendent" Dockerfile commands and can be replaced inline in many as
    -- well.
    --
    --
    --  Syntax:
    --
    -- @
    -- ENV <key> <value>
    -- ENV <key>=<value> ...
    -- @
    --
    -- The second form allows multiple key value pairs to be specified
    --
    -- @
    --  ENV myName="John Doe" myDog=Rex\ The\ Dog \
    --      myCat=fluffy
    --  and
    --
    --  ENV myName John Doe
    --  ENV myDog Rex The Dog
    --  ENV myCat fluffy
    -- @

  | Add FilePath FilePath
    -- ^ The ADD instruction copies new files, directories or remote file URLs
    -- from <src> and adds them to the filesystem of the container at the
    -- path <dest>.
    --
    -- Syntax:
    --
    -- ADD <src>... <dest>
    -- ADD ["<src>"... "<dest>"] (this form is required for paths containing whitespace)
    --
    -- The ADD instruction copies new files, directories or remote file URLs
    -- from <src> and adds them to the filesystem of the container at the
    -- path <dest>.

  | Copy String FilePath
    -- ^
    -- COPY has two forms:
    --
    -- COPY <src>... <dest>
    -- COPY ["<src>"... "<dest>"] (this form is required for paths containing whitespace)
    --
    -- The COPY instruction copies new files or directories from <src>
    -- and adds them to the filesystem of the container at the path <dest>.

  | Entrypoint String [Param]
    -- ^ An ENTRYPOINT allows you to configure a container that will run as
    -- an executable.
    --
    -- @
    -- ENTRYPOINT ["executable", "param1", "param2"] (the preferred exec form)
    -- ENTRYPOINT command param1 param2 (shell form)
    -- @

  | Volume [FilePath]
    -- ^ @ VOLUME ["/data"] @
    --
    -- The VOLUME instruction creates a mount point with the specified
    -- name and marks it as holding externally mounted volumes from native
    -- host or other containers.
  | User String
    -- ^ USER daemon
    --
    -- The USER instruction sets the user name or UID to use when running the
    -- image and for any RUN, CMD and ENTRYPOINT instructions that follow it
    -- in the Dockerfile.

  | WorkDir FilePath
    -- ^ The WORKDIR instruction sets the working directory for any RUN, CMD,
    -- ENTRYPOINT, COPY and ADD instructions that follow it in the
    -- Dockerfile.
    --
    -- @ WORKDIR /path/to/workdir @

  | OnBuild Instruction
    -- ^ The ONBUILD instruction adds to the image a trigger
    -- instruction to be executed at a later time, when the image is used as
    -- the base for another build. The trigger will be executed in the
    -- context of the downstream build, as if it had been inserted
    -- immediately after the FROM instruction in the downstream Dockerfile.
    --
    -- @
    -- ^
    -- [...]
    -- ONBUILD ADD . /app/src
    -- ONBUILD RUN /usr/local/bin/python-build --dir /app/src
    -- [...]
    -- @
  deriving Show

prettyCmd                    :: Instruction -> String
prettyCmd (From f)           = "FROM " ++ f
prettyCmd (Maintainer m)     = "MAINTAINER " ++ m
prettyCmd (Run scr)          = "RUN " ++ scr
prettyCmd (Cmd cmds)         = "CMD " ++ show cmds
prettyCmd (Label k (Just v)) = "LABEL " ++ k ++ "=" ++ v
prettyCmd (Label k Nothing)  = "LABEL " ++ k
prettyCmd (Expose p)         = "EXPOSE " ++ show p
prettyCmd (Env k v)          = "ENV " ++ k ++ " " ++ v
prettyCmd (Add s d)          = "ADD " ++ s ++ " " ++ d
prettyCmd (Copy s d)         = "COPY " ++ s ++ " " ++ d
prettyCmd (Entrypoint e ps)  = "ENTRYPOINT " ++ show (e:ps)
prettyCmd (Volume vs)        = "VOLUME " ++ show vs
prettyCmd (User u)           = "USER " ++ u
prettyCmd (WorkDir cwd)      = "WORKDIR " ++ cwd
prettyCmd (OnBuild instr)    = error ("ONBUILD "  ++ "is not currently supported.")


-- * Instructions

from :: String -> Docker ()
from f = tell [ From f ]

maintainer :: String -> Docker ()
maintainer m = tell [ Maintainer m ]

-- | TODO support alternate forms
-- run scr ps = tell [ Run scr ps ]
run :: Script -> Docker ()
run scr = tell [ Run scr ]

env :: String -> String -> Docker ()
env k v = tell [ Env k v ]

cmd :: [ScriptFile] -> Docker ()
cmd cs = tell [ Cmd cs ]

expose :: Int -> Docker ()
expose p = tell [ Expose p ]

add :: FilePath -> FilePath -> Docker ()
add k v = tell [ Add k v ]

copy :: FilePath -> FilePath -> Docker ()
copy s d = tell [ Copy s d ]

entrypoint :: FilePath -> [Param] -> Docker ()
entrypoint e ps = tell [ Entrypoint e ps ]

volume :: [FilePath] -> Docker ()
volume vs  = tell [ Volume vs ]

user :: String -> Docker ()
user u = tell [ User u ]

workdir :: FilePath -> Docker ()
workdir cwd = tell [ WorkDir cwd ]

onbuild :: Instruction -> Docker ()
onbuild c = error "OnBuild instruction is not yet supported"

