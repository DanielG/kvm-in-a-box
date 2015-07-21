module Options where

import Options.Applicative
import Options.Applicative.Types
import Data.Maybe
import Data.Monoid
import qualified Data.Set as Set

import Types

data Options = Options { oRoot :: FilePath }

exceptP :: Either String a -> ReadM a
exceptP (Left s)  = readerError s
exceptP (Right a) = return a

intOption :: Mod OptionFields Int -> Parser Int
intOption = option auto

defaultOpt x f = fromMaybe x <$> optional f

vmP :: Parser VmFlags
vmP = VmFlags <$> vmSSP <*> vmVSP

vmSSP :: Parser VmSSFlags
vmSSP = VmSSFlags
     <$> optional $$ strOption $$
                 long "vg"
            <<>> metavar "LVM_VG"
            <<>> help "LVM volume group this vm resides on"

vmVSP :: Parser VmVSFlags
vmVSP = VmVSFlags
     <$> optional $$ intOption $$
               long "cpus"
            <<>> metavar "CORES"
            <<>> help "How many cpu cores to give the VM"
     <*> optional $$ intOption $$
               long "mem"
            <<>> metavar "MB"
            <<>> help "How many of memory [MB] to give the VM"
     <*> optional $$ strOption $$
               long "arch"
            <<>> metavar "ARCH"
            <<>> help "Which CPU architecture the VM should use"

     <*> Just <$$> switch $$
                 long "user"
            <<>> help "Enable the QEMU 'user' network interface"

     <*> Just <$$> switch $$
                 long "public"
            <<>> help "Enable the public network interface"

     <*> Just <$$> switch $$
                 long "private"
            <<>> help "Enable the private network interface"

     <*> optional $$ (Set.fromList <$> (some $$ strOption $$
                 long "net-group"
            <<>> metavar "ID"
            <<>> help "Attach VM to specified netork switch"))


optionsP :: Parser Options
optionsP = Options
       <$> fromMaybe "/" <$$> optional $$ strOption $$
             long "root"
        <<>> metavar "ROOT"
        <<>> help "Use this directory instead of '/'"

infixl 6 <||>
infixl 7 <$$>
infixr 8 $$
infixr 9 <<>>

($$) = ($)
(<||>) = (<|>)
(<<>>) = (<>)
(<$$>) = (<$>)

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t
