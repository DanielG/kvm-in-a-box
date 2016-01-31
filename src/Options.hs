{-# LANGUAGE CPP #-}
module Options where

import Options.Applicative
import Options.Applicative.Types
import Data.Word
import Data.Maybe
import Data.Monoid
import Data.List.Split
import qualified Data.Set as Set

import Types
import Read

data Options = Options { oRoot :: FilePath, oQuiet :: Bool } deriving Show

exceptP :: Either String a -> ReadM a
exceptP (Left s)  = readerError s
exceptP (Right a) = return a

intOption :: Mod OptionFields Int -> Parser Int
intOption = option $ eitherReader $ (\str' -> readEither str')

readIntegralSafe :: forall a. (Integral a, Read a, Bounded a) => String -> Either String a
readIntegralSafe str = let
    i = read str :: Integer
    min = toInteger (minBound :: a)
    max = toInteger (maxBound :: a)
  in if i < min || i > max
       then Left $ "readIntegralSafe: out of bounds, should be between "++show min++" and "++show max
       else Right $ fromIntegral i

parseIntegralSafe :: (Integral a, Read a, Bounded a) => String -> ReadM a
parseIntegralSafe str =
    case readIntegralSafe str of
      Right x -> return x
      Left err -> readerError err

portOptionP :: Mod OptionFields (Proto, Word16) -> Parser (Proto, Word16)
portTupOptionP :: Mod OptionFields (Proto, (Word16, Word16)) -> Parser (Proto, (Word16, Word16))

#if MIN_VERSION_optparse_applicative(0,11,0)
portOptionP = option $ str >>= portOptionP'
portTupOptionP = option $ str >>= portTupOptionP'
#else
portOptionP = option portOptionP'
portTupOptionP = option portTupOptionP'
#endif

portOptionP' :: String -> ReadM (Proto, Word16)
portOptionP' str = do
  case splitOn ":" str of
    [mkProto -> proto, port] -> (proto,) <$> parseIntegralSafe port
    _ -> readerError "portOptionP: expecting format 'PROTO:PORT'"

portTupOptionP' :: String -> ReadM (Proto, (Word16, Word16))
portTupOptionP' str = do
  case splitOn ":" str of
    [mkProto -> proto, intern, extern] ->
        (proto,) <$> ((,) <$> parseIntegralSafe intern <*> parseIntegralSafe extern)
    _ -> readerError "portTupOptionP: expecting format 'PROTO:IPORT:EPORT'"


defaultOpt x f = fromMaybe x <$> optional f

vmP :: Parser VmFlags
vmP = VmFlags <$> vmCfgP <*> vmSysCfgP <*> vmNetCfgP <*> vmQCfgP

vmCfgP :: Parser VmCfgFlags
vmCfgP = VmCfgFlags
     <$> optional $$ strOption $$
               long "arch"
            <<>> metavar "ARCH"
            <<>> help "Which CPU architecture the VM should use"

vmSysCfgP :: Parser VmSysCfgFlags
vmSysCfgP = VmSysCfgFlags
     <$> optional $$ strOption $$
                 long "vg"
            <<>> metavar "LVM_VG"
            <<>> help "LVM volume group this vm resides on"

     <*> optional $$ some $$ strOption $$
                 long "add-disk"
            <<>> metavar "LVM_LV"
            <<>> help "Add a secondary disk: LVM_VG/VM_NAME-LVM_LV"

     <*> optional $$ some $$ strOption $$
                 long "ssh-key"
            <<>> metavar "ssh-rsa AAAA ...."
            <<>> help "SSH public key string to add to VM console's authorized_keys"

vmNetCfgP :: Parser VmNetCfgFlags
vmNetCfgP = VmNetCfgFlags
     <$> boolP "user" "the QEMU 'user' network interface"
     <*> boolP "public" "the public network interface"
     <*> boolP "private" "the private network interface"

     <*> optional $$ (Set.fromList <$> (some $$ strOption $$
                 long "net-group"
            <<>> metavar "ID"
            <<>> help "Attach VM to specified netork switch"))

     <*> optional $$ some $$ portTupOptionP $$
                 long "forward4"
            <<>> metavar "PROTO:IPORT:EPORT"
            <<>> help "NAT port forwarding IPORT at VM, EPORT from beyond the firewall"

     <*> optional $$ some $$ portOptionP $$
                 long "open6"
            <<>> metavar "PROTO:PORT"
            <<>> help "Open a port on the v6 firewall"


vmQCfgP :: Parser VmQCfgFlags
vmQCfgP = VmQCfgFlags
     <$> optional $$ intOption $$
               long "cpus"
            <<>> metavar "CORES"
            <<>> help "How many cpu cores to give the VM"
     <*> optional $$ intOption $$
               long "mem"
            <<>> metavar "MB"
            <<>> help "How many of memory [MB] to give the VM"

boolP n doc = optional $
       flag' True $$ long n <<>> help ("Enable " ++ doc)
  <||> flag' False $$ long ("no-"++n) <<>> help ("Disable " ++ doc)

optionsP :: Parser Options
optionsP = Options
       <$> fromMaybe "/" <$$> optional $$ strOption $$
             long "root"
        <<>> metavar "ROOT"
        <<>> help "Use this directory instead of '/'"

       <*> switch $$
             short 'q'
        <<>> long "quiet"
        <<>> help "Be quiet"



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
