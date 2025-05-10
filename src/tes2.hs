import Control.Monad
import Data.Binary
import Data.Binary.Get
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

data RecordType = GMST | GLOB | CLAS | FACT | HAIR | EYES | RACE | SOUN | SKIL | MGEF | SCPT | LTEX | ENCH | SPEL | BSGN | ACTI | APPA | ARMO | BOOK | CLOT | CONT | DOOR | INGR | LIGH | MISC | STAT | GRAS | TREE | FLOR | FURN | WEAP | AMMO | NPC_ | CREA | LVLC | SLGM | KEYM | ALCH | SBSP | SGST | LVLI | WTHR | CLMT | REGN | CELL | WRLD | DIAL | QUST | IDLE | PACK | CSTY | LSCR | LVSP | ANIO | WATR | EFSH deriving (Read, Show, Eq)

type FormID = Word32
type Size = Word32

data GoR a b = G a | R b 
type GroupOrRecord = GoR Group Record

extractG (G g) = g

instance (Show a, Show b) => Show (GoR a b) where
	show (G g) = show g
	show (R r) = show r

data Group = Group GroupType Size [GroupOrRecord] 
instance Show Group where
	show (Group t s ls) = "\n<group type=\"" ++ show t ++ "\" size=\"" ++ show t ++ "\">" ++ concatMap show ls ++ "\n</group>"
getChildren (Group a s c) = c

data GroupType = Top String | WorldChildren Word32 | IntCellBlock Word32 | IntCellSubBlock Word32 | ExtCellBlock (Int16, Int16) | ExtCellSubBlock (Int16, Int16) | CellChildren FormID | TopicChildren FormID | CellPersistentChildren FormID | CellTemporaryChildren FormID | CellVisibleDistantChildren FormID 
instance Show GroupType where
	show ( Top a ) = "Top "++ a
	show ( WorldChildren a ) = "WorldChildren " ++ show a 
	show ( IntCellBlock a ) = "IntCellBlock " ++ show a 
	show ( IntCellSubBlock a ) = "IntCellSubBlock " ++ show a 
	show ( ExtCellBlock a ) = "ExtCellBlock " ++ show a 
	show ( ExtCellSubBlock a ) = "ExtCellSubBlock " ++ show a 
	show ( CellChildren a ) = "CellChildren " ++ show a 
	show ( TopicChildren a ) = "TopicChildren " ++ show a 
	show ( CellPersistentChildren a ) = "CellPersistentChildren " ++ show a 
	show ( CellTemporaryChildren a ) = "CellTemporaryChildren " ++ show a 
	show ( CellVisibleDistantChildren a ) = "CellVisibleDistantChildren " ++ show a 

data RecordData a = SR [a] | Compressed 
instance (Show a) => Show (RecordData a) where
	show (SR a) = concatMap show a
	show Compressed = "<compressed/>"

data Record = Record String Size (RecordData SubRecord)
instance Show Record where
	show (Record s z sub) = "\n\t<record type=\"" ++ s ++ "\">" ++ show sub ++ "\n\t</record>"

data SubRecord = Muu String Size | XCLW Float | XCLC (Int32, Int32) | FULL String | EDID String
instance Show SubRecord where
	show (Muu s z) = "\n\t\t<" ++ map toLower s ++ "/>"
	show (XCLW f) = "\n\t\t<xclw height=\"" ++ show f ++ "\"/>"
	show (XCLC (a,b)) = "\n\t\t<xclc x=\"" ++ show a ++ "\" y=\"" ++ show b ++ "\" />"
	show (FULL s) = "\n\t\t<full name=\"" ++ s ++ "\" />"
	show (EDID s) = "\n\t\t<edid id=\"" ++ s ++ "\" />"

data SubRecordData a = CellData a | Other

instance (Binary a, Binary b) => Binary (GoR a b) where
	put = undefined
	get = do
		tag <- liftM identifier (lookAhead getWord32le)
		case tag of
			"GRUP" -> liftM G get
			_ -> liftM R get

instance Binary Group where
	put = undefined
	get = do
		tag <- liftM identifier getWord32le
		size <- getWord32le
		label <- getWord32le
		groupType <- liftM (unsafeCoerce :: Word32 -> Int32) getWord32le
		stamp <- getWord32le
		let gtype = case groupType of
			0 -> Top $ identifier label
			1 -> WorldChildren label 
			2 -> IntCellBlock label 
 			3 -> IntCellSubBlock label 
 			4 -> ExtCellBlock $ coordPair label 
			5 -> ExtCellSubBlock $ coordPair label 
			6 -> CellChildren label 
			7 -> TopicChildren label 
			8 -> CellPersistentChildren label 
			9 -> CellTemporaryChildren label 
			10 -> CellVisibleDistantChildren label
			a -> error $ "Unknown grouptype: " ++ show a
		case gtype of
			Top _ -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getAll temp :: [GroupOrRecord]
				return $ Group gtype size children
			WorldChildren _ -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getAll temp
				return $ Group gtype size children
	{-		ExtCellBlock _ -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getGroups temp
				return $ Group gtype size children
			ExtCellSubBlock _ -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getGroups temp
				return $ Group gtype size children -}
			_ -> do
				skip $ fromIntegral size - 20
				return $ Group gtype size []

instance Binary Record where
	put = undefined
	get = do
		rtype <- liftM (identifier) getWord32le
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
				let subs = runGet getAll temp :: [SubRecord]
				return $ Record rtype datasize (SR subs)

instance Binary SubRecord where
	put = undefined
	get = do
		tag' <- liftM (identifier) getWord32le
		size' <- liftM fromIntegral getWord16le
		(tag, size) <- case tag' of
			"XXXX" -> do
				size <- getWord32le
				tag <- liftM (identifier) getWord32le
				skip 2
				return (tag, size)
			_ -> return (tag', size')
		case tag of
			"XCLW" -> do
				wl <- getFloat32le
				return $ XCLW wl
			"XCLC" -> do
				x <- getWord32le
				y <- getWord32le
				return $ XCLC (fromIntegral x,fromIntegral y)
			"FULL" -> do
				s <- liftM CL.unpack $ getLazyByteStringNul -- getByteString $ fromIntegral $ size - 1
				return $ FULL s
			"EDID" -> do
				s <- liftM CL.unpack $ getLazyByteStringNul -- getByteString $ fromIntegral $ size - 1
				return $ EDID s
			_ ->  do
				skip (fromIntegral size)
				return $ Muu tag (fromIntegral size)

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
		rest <- getAll
		return (first : rest)

main = do
	input <- BL.readFile "oblivion.esm"
	let groups = runGet (skip 764 >> getAll) input :: [GroupOrRecord]
	print $ groups!!45

