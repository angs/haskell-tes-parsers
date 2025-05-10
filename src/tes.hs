import Control.Monad
import Data.Binary
import Data.Binary.IEEE754
import Data.Binary.Get
import Data.Word
import Data.Int
import Data.Bits
import Unsafe.Coerce
import qualified Data.ByteString.Lazy as BL

data RecordType = GMST | GLOB | CLAS | FACT | HAIR | EYES | RACE | SOUN | SKIL | MGEF | SCPT | LTEX | ENCH | SPEL | BSGN | ACTI | APPA | ARMO | BOOK | CLOT | CONT | DOOR | INGR | LIGH | MISC | STAT | GRAS | TREE | FLOR | FURN | WEAP | AMMO | NPC_ | CREA | LVLC | SLGM | KEYM | ALCH | SBSP | SGST | LVLI | WTHR | CLMT | REGN | CELL | WRLD | DIAL | QUST | IDLE | PACK | CSTY | LSCR | LVSP | ANIO | WATR | EFSH deriving (Read, Show, Eq)

type FormID = Word32
type Size = Word32

data GroupOrRecord a b = G a | R b deriving Show

data Group = Group GroupType Size [GroupOrRecord Group Record] deriving Show

data GroupType = Top String | WorldChildren Word32 | IntCellBlock Word32 | IntCellSubBlock Word32 | ExtCellBlock (Int, Int) | ExtCellSubBlock (Int, Int) | CellChildren FormID | TopicChildren FormID | CellPersistenChildren FormID | CellTemporaryChildren FormID | CellVisibleDistantChildren FormID deriving Show

data RecordData a = SR [a] | Compressed deriving Show

data Record = Record String Size (RecordData SubRecord) deriving Show

data SubRecord = Muu String Size | XCLW Float | XCLC (Int32, Int32) deriving Show

data SubRecordData a = CellData a | Other

instance (Binary a, Binary b) => Binary (GroupOrRecord a b) where
	put = undefined
	get = do
		tag <- liftM char4 (lookAhead getWord32le)
		case tag of
			"GRUP" -> liftM G get
			_ -> liftM R get

instance Binary Group where
	put = undefined
	get = do
		tag <- liftM char4 getWord32le
		size <- getWord32le
		label <- getWord32le
		groupType <- liftM (unsafeCoerce :: Word32 -> Int32) getWord32le
		stamp <- getWord32le
		let gtype = case groupType of
			0 -> Top $ char4 label
			1 -> WorldChildren label 
			2 -> IntCellBlock label 
 			3 -> IntCellSubBlock label 
 			4 -> ExtCellBlock $ short2 label 
			5 -> ExtCellSubBlock $ short2 label 
			6 -> CellChildren label 
			7 -> TopicChildren label 
			8 -> CellPersistenChildren label 
			9 -> CellTemporaryChildren label 
			10 -> CellVisibleDistantChildren label
			a -> error $ "Unknown grouptype: " ++ show a
		case gtype of
			Top "WRLD" -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getGroups temp
				return $ Group gtype size children
			WorldChildren _ -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getGroups temp
				return $ Group gtype size children
			ExtCellBlock _ -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getGroups temp
				return $ Group gtype size children
			ExtCellSubBlock _ -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getGroups temp
				return $ Group gtype size children
			_ -> do
				skip $ fromIntegral size - 20
				return $ Group gtype size []

instance Binary Record where
	put = undefined
	get = do
		rtype <- liftM (char4) getWord32le
		datasize <- getWord32le
		flags <- getWord32le
		let compressed = testBit flags 18
		formid <- getWord32le
		vci <- getWord32le
		if compressed then do
			skip (fromIntegral datasize)
			return (Record rtype datasize Compressed)
			else do
				temp <- getLazyByteString (fromIntegral datasize)
				let subs = runGet getSubs temp
				return $ Record rtype datasize (SR subs)

instance Binary SubRecord where
	put = undefined
	get = do
		tag <- liftM (char4) getWord32le
		size <- getWord16le
		if size /= 0 then do
			case tag of
				"XCLW" -> do
					wl <- getFloat32le
					return $ XCLW wl
				"XCLC" -> do
					x <- getWord32le
					y <- getWord32le
					return $ XCLC (fromIntegral x,fromIntegral y)
				_ ->  do
					skip (fromIntegral size)
					return $ Muu tag (fromIntegral size)
			else do
			let size = 70200
			skip (fromIntegral size)
			return $ Muu tag (fromIntegral size)

char4 :: Word32 -> String
char4 w = map (toEnum . fromIntegral) [a,b,c,d]
	where
	(w',  a) = quotRem w 256 
	(w'', b) = quotRem w' 256
	(d  , c) = quotRem w'' 256

short2 :: Word32 -> (Int, Int)
short2 w = let (a,b) = quotRem w 65536 in (fromIntegral a, fromIntegral b)

main = do
	input <- BL.readFile "oblivion.esm"
	let groups = runGet (skip 764 >> getGroups) input
	print $ groups!!45

getGroups = do
	empty <- isEmpty
	if empty then return [] else do
		group <- get :: Get (GroupOrRecord Group Record)
		groups <- getGroups
		return (group : groups)

getSubs = do
	empty <- isEmpty
	if empty then return [] else do
		group <- get :: Get SubRecord
		groups <- getSubs
		return (group : groups)

