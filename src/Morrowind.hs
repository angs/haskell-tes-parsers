module Morrowind (getCoords) where 

import Codec.Compression.Zlib
import Text.XML.HXT.Core hiding (NAME)
import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.List.Split
import Data.Binary.IEEE754
import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import qualified Control.Monad.State as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import Unsafe.Coerce

type FormID = Word32
type Size = Word32

data Record = Record String Size [SubRecord]
recordType (Record a _ _) = a
data SubRecord 
	= Muu String Size 
	| FNAM String
	| NAME String
	| FRMR Word32
	| Object [SubRecord]
	| CELLDATA Word32 Int32 Int32
	| OBJDATA Float Float Float Float Float Float
	| NPCO Word32 String
	| RGNN String
	| VHGT Float Word8 [[Word8]] Word16
	| INTV Int32 Int32

exteriorCell (Record _ _ subs) = even flags
	where
	isCellData (CELLDATA _ _ _) = True
	isCellData _ = False
	CELLDATA flags _ _ = 	head $ filter isCellData subs

instance Show Record where
	show (Record s z sub) = "\n<record type=\"" ++ s ++ "\" size=\"" ++ show z ++ "\">" ++ concatMap show sub ++ "\n</record>"

instance Show SubRecord where
	show (Muu s z) = "\n\t\t<" ++ map toLower s ++ " size=\" "++ show z++"\" />"
	show (NAME f) = "\n\t\t<name id=\"" ++ f ++ "\"/>"
	show (FRMR x) = "\n\t\t<frmr index=\"" ++ show x ++ "\" />"
	show (FNAM s) = "\n\t\t<fnam name=\"" ++ s ++ "\" />"
	show (RGNN s) = "\n\t\t<rgnn name=\"" ++ s ++ "\" />"
	show (Object s) = "\n\t<object>" ++ concatMap show s ++ "\n\t</object>"
	show (CELLDATA flags x y) = "\n\t\t<celldata flags=\""++ show flags ++ "\" x=\"" ++ show x ++"\" y=\"" ++ show y ++ "\" />"
	show (OBJDATA x y _ _ _ _) = "\n\t\t<objdata x=\"" ++ show x ++"\" y=\"" ++ show y ++ "\" />"
	show (NPCO s id) = "\n\t\t<npco count=\"" ++ show s ++ "\" id="++ show id++" />"
	show (INTV x y) = "\n\t\t<intv x=\"" ++ show x ++ "\" y="++ show y++" />"
	show (VHGT o _ g _) = "\n\t\t<vhgt offset=\"" ++ show o ++ "\" grid="++ show g++" />"

instance Binary Record where
	put = undefined
	get = do
		rtype    <- getIdentifier
		size     <- getWord32le
		header1  <- getWord32le
		flags    <- getWord32le
		subdata  <- getLazyByteString (fromIntegral size)
		let subs = runGet (getAllWith $ getSubRecord rtype) subdata :: [SubRecord]
		return $ Record rtype size subs

getObject :: Get SubRecord
getObject = do 
	lista <- go
	return $ Object lista
	where
	go = do
		empty <- isEmpty
		if empty then return [] else do
			tag <- lookAhead getIdentifier
			if tag == "FRMR" then return [] else do
				s <- getSubRecord "OBJE"
				loput <- go
				return $ s:loput 

getSubRecord rtype = do
		tag        <- getIdentifier
		size       <- getWord32le
		case (tag,rtype) of
			("DATA" , "CELL" ) -> CELLDATA <$> getWord32le <*> getInt32le <*> getInt32le
			("DATA" , "OBJE" ) -> OBJDATA <$> getFloat32le <*> getFloat32le <*> getFloat32le <*> getFloat32le <*> getFloat32le <*> getFloat32le
			("FRMR" , _      ) -> skip 4 >> getObject --FRMR <$> getWord32le
			("NAME" , _      ) -> NAME <$> getString size
			("FNAM" , _      ) -> FNAM <$> getString size
			("RGNN" , _      ) -> RGNN <$> getString size
			("NPCO" , _      ) -> NPCO <$> getWord32le <*> (liftM (takeWhile (/='\NUL')) $ getString 32)
			("VHGT" , "LAND" ) -> VHGT <$> getFloat32le <*> getWord8 <*> (chunksOf 65 <$> replicateM (65*65) getWord8) <*> getWord16le
			("INTV" , "LAND" ) -> INTV <$> getInt32le <*> getInt32le
			_      -> do
				skip (fromIntegral size)
				return $ Muu tag (fromIntegral size)

getStringNul     = liftM CL.unpack getLazyByteStringNul
getString n = (liftM CL.unpack $ getLazyByteString $ fromIntegral n - 1) >>= \a -> skip 1 >> return a
getIdentifier = liftM identifier getWord32le
--getInt32le    = liftM fromIntegral getWord32le

parseObjects :: [SubRecord] -> [SubRecord]
parseObjects obs = go [] obs
	where
	go accum (he@(FRMR _) : loput) = case accum of
		[] -> go [he] loput
		accum -> Object (reverse accum) : go [he] loput
	go accum (he : loput) = case accum of
		[] -> he : go [] loput
		accum -> go (he:accum) loput
	go []  [] = []
	go acc [] = [Object acc]

identifier :: Word32 -> String
identifier w = map (toEnum . fromIntegral) [a,b,c,d]
	where
	(w'  , a) = quotRem w   256 
	(w'' , b) = quotRem w'  256
	(d   , c) = quotRem w'' 256

coordPair :: Word32 -> (Int16, Int16)
coordPair w = let (a,b) = quotRem w 65536 in (fromIntegral a, fromIntegral b)

getAll :: Binary a => Get [a]
getAll = do
	empty <- isEmpty
	if empty then return [] else do
		first <- get
		rest  <- getAll
		return (first : rest)

getAllWith :: Get a -> Get [a]
getAllWith getf = do
	empty <- isEmpty
	if empty then return [] else do
		first <- getf
		rest  <- getAllWith getf
		return (first : rest)

main2 = do
	input <- BL.readFile "morrowind.esm"
	let records = filter exteriorCell $ filter ((=="CELL").recordType) $ runGet (getAll) input :: [Record]
	mapM_ print $ listOfCells $ "<root>" ++ concatMap show records ++ "</root>"
	--putStrLn $ concatMap show records

getCoords = do
	input <- BL.readFile "morrowind.esm"
	let records = filter ((=="LAND").recordType) $ runGet (getAll) input :: [Record]
	--mapM_ print $ listOfCells $ "<root>" ++ concatMap show records ++ "</root>"
	putStrLn $ concatMap show records

listOfItems xml = 
	let 
		a = getChildren >>> (getCellCoords &&& getObjNameAndCoords)
		getCellCoords = getChildren >>> hasName "celldata" >>> (getAttrValue "x" &&& getAttrValue "y")
		getObjNameAndCoords = getChildren >>> hasName "object" >>> ((getChildren >>> hasName "name" >>> getAttrValue "id") &&& (getChildren >>> hasName "objdata" >>> (getAttrValue "x" &&& getAttrValue "y" )))
		in (runLA $ hread >>> a) xml

listOfCells xml = 
	let 
		a = getChildren >>> (getCellCoords)
		getCellCoords = getChildren >>> hasName "celldata" >>> (getAttrValue "x" &&& getAttrValue "y" &&& getAttrValue "height")
		in (runLA $ hread >>> a) xml
