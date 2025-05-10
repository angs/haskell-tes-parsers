import Text.XML.HXT.Core
import Codec.Compression.Zlib
import Control.Applicative
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

data GroupType 
	= Top String 
	| WorldChildren Word32 
	| IntCellBlock Word32 
	| IntCellSubBlock Word32 
	| ExtCellBlock (Int16, Int16) 
	| ExtCellSubBlock (Int16, Int16) 
	| CellChildren FormID 
	| TopicChildren FormID 
	| CellPersistentChildren FormID 
	| CellTemporaryChildren FormID 
	| CellVisibleDistantChildren FormID 

instance Show GroupType where
	show ( Top a )                        = "Top "                        ++ a
	show ( WorldChildren a )              = "WorldChildren "              ++ show a
	show ( IntCellBlock a )               = "IntCellBlock "               ++ show a
	show ( IntCellSubBlock a )            = "IntCellSubBlock "            ++ show a
	show ( ExtCellBlock a )               = "ExtCellBlock "               ++ show a
	show ( ExtCellSubBlock a )            = "ExtCellSubBlock "            ++ show a
	show ( CellChildren a )               = "CellChildren "               ++ show a
	show ( TopicChildren a )              = "TopicChildren "              ++ show a
	show ( CellPersistentChildren a )     = "CellPersistentChildren "     ++ show a
	show ( CellTemporaryChildren a )      = "CellTemporaryChildren "      ++ show a
	show ( CellVisibleDistantChildren a ) = "CellVisibleDistantChildren " ++ show a

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
	show (Group (Top t) s c) = "<top label=\"" ++ t ++ "\" size=\"" ++ show s ++ "\">" ++ concatMap show c ++ "</top>"
	show (Group (WorldChildren t) s c) = "<worldchildren label=\"" ++ show t ++ "\" size=\"" ++ show s ++ "\">" ++ concatMap show c ++ "</worldchildren>\n"
	show (Group (IntCellBlock t) s c) = "<intcellblock label=\"" ++ show t ++ "\" size=\"" ++ show s ++ "\">" ++ concatMap show c ++ "</intcellblock>\n"
	show (Group (IntCellSubBlock t) s c) = "<intcellsubblock label=\"" ++ show t ++ "\" size=\"" ++ show s ++ "\">" ++ concatMap show c ++ "</intcellsubblock>\n"
	show (Group (ExtCellBlock t) s c) = "<extcellblock label=\"" ++ show t ++ "\" size=\"" ++ show s ++ "\">" ++ concatMap show c ++ "</extcellblock>\n"
	show (Group (ExtCellSubBlock t) s c) = "<extcellsubblock label=\"" ++ show t ++ "\" size=\"" ++ show s ++ "\">" ++ concatMap show c ++ "</extcellsubblock>\n"
	show (Group (CellChildren t) s c) = "<cellchildren label=\"" ++ show t ++ "\" size=\"" ++ show s ++ "\">" ++ concatMap show c ++ "</cellchildren>\n"
	show (Group (TopicChildren t) s c) = "<topicchildren label=\"" ++ show t ++ "\" size=\"" ++ show s ++ "\">" ++ concatMap show c ++ "</topicchildren>\n"
	show (Group (CellPersistentChildren t) s c) = "<cellpersistentchildren label=\"" ++ show t ++ "\" size=\"" ++ show s ++ "\">" ++ concatMap show c ++ "</cellpersistentchildren>\n"
	show (Group (CellTemporaryChildren t) s c) = "<celltemporarychildren label=\"" ++ show t ++ "\" size=\"" ++ show s ++ "\">" ++ concatMap show c ++ "</celltemporarychildren>\n"
	show (Group (CellVisibleDistantChildren t) s c) = "<cellvisibledistantchildren label=\"" ++ show t ++ "\" size=\"" ++ show s ++ "\">" ++ concatMap show c ++ "</cellvisibledistantchildren>\n"
	--show (Group t s ls) = "\n<group type=\"" ++ show t ++ "\" size=\"" ++ show t ++ "\">" ++ concatMap show ls ++ "\n</group>"

--getChildren (Group a s c) = c

data Record 
	= Record String Size [SubRecord] (Maybe Group)

instance Show Record where
	show (Record s z sub (Just childgroup)) = "<record type = \"" ++ s ++ "\">" ++ concatMap show sub ++ show childgroup ++ "</record>\n"
	show (Record s z sub Nothing)           = "<record type = \"" ++ s ++ "\">" ++ concatMap show sub ++ "</record>\n"

{-	show (World z sub Nothing) = "<record type=\"WRLD\" size=\""++show z++"\">" ++ concatMap show sub ++ "</record>\n"
	show (Cell z sub Nothing) = "<record type=\"CELL\" size=\""++show z++"\">" ++ concatMap show sub ++ "</record>\n"
	show (World z sub (Just cw)) = "<record type=\"WRLD\" size=\""++show z++"\">" ++ concatMap show sub ++ show cw ++ "</record>\n"
	show (Cell z sub (Just cw)) = "<record type=\"CELL\" size=\""++show z++"\">" ++ concatMap show sub ++ show cw ++ "</record>\n"
		| s == "CELL" = "\n\t<record type=\"" ++ s ++ "\">" ++ concatMap show sub ++ "\n\t</record>"
		| otherwise = ""
-}


data SubRecord 
	= Muu String Size 
	| XCLW Float 
	| XCLC Int32 Int32 
	| FULL String 
	| EDID String
instance Show SubRecord where
	show (Muu s z)  = "<" ++ map toLower s ++ " />\n"
	show (XCLW f)   = "<xclw height=\"" ++ show f ++ "\" />\n"
	show (XCLC x y) = "<xclc x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\" />\n"
	show (FULL s)   = "<full name=\"" ++ stringEscapeXml s ++ "\" />\n"
	show (EDID s)   = "<edid id=\"" ++ s ++ "\" />\n"


instance (Binary a, Binary b) => Binary (GoR a b) where
	put = undefined
	get = do
		tag <- lookAhead getIdentifier
		case tag of
			"GRUP" -> liftM G get
			_ -> liftM R get

instance Binary Group where
	put = undefined
	get = do
		tag       <- getIdentifier
		size      <- getWord32le
		label     <- getWord32le
		groupType <- getWord32le
		stamp     <- getWord32le
		let gtype = case groupType of
			0  -> Top                        $ identifier label
			1  -> WorldChildren              $ label
			2  -> IntCellBlock               $ label
 			3  -> IntCellSubBlock            $ label
 			4  -> ExtCellBlock               $ coordPair label
			5  -> ExtCellSubBlock            $ coordPair label
			6  -> CellChildren               $ label
			7  -> TopicChildren              $ label
			8  -> CellPersistentChildren     $ label
			9  -> CellTemporaryChildren      $ label
			10 -> CellVisibleDistantChildren $ label
			a  -> error $ "Unknown grouptype: " ++ show a
		case gtype of
			Top _ -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getAll temp :: [GroupOrRecord]
				return $ Group gtype size children
			WorldChildren _ -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getAll temp :: [GroupOrRecord]
				return $ Group gtype size children
			CellChildren _ -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getAll temp :: [GroupOrRecord]
				return $ Group gtype size children
			ExtCellBlock _ -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getAll temp :: [GroupOrRecord]
				return $ Group gtype size children
			ExtCellSubBlock _ -> do
				temp <- getLazyByteString (fromIntegral $ size-20)
				let children = runGet getAll temp :: [GroupOrRecord]
				return $ Group gtype size children 
			_ -> do
				skip $ fromIntegral size - 20
				return $ Group gtype size []

instance Binary Record where
	put = undefined
	get = do
		rtype    <- getIdentifier
		datasize <- getWord32le
		flags    <- getWord32le
		formid   <- getWord32le
		vci      <- getWord32le
		let compressed = testBit flags 18
		subdata  <- if compressed then do
			skip 4
			liftM decompress $  getLazyByteString (fromIntegral datasize - 4)
			else getLazyByteString (fromIntegral datasize)
		let subs = runGet getAll subdata :: [SubRecord]
		if rtype `elem` ["WRLD", "CELL", "DIAL"] then do
			empty <- isEmpty
			if empty then return $ Record rtype datasize subs Nothing else do
				hasSubGroup <- lookAhead (liftM (=="GRUP") getIdentifier)
				if hasSubGroup then Record rtype datasize subs <$> liftM Just get else return $ Record rtype datasize subs Nothing
			else return $ Record rtype datasize subs Nothing
		{-
			"WRLD" -> do
				empty <- isEmpty
				if empty then return $ World datasize subs Nothing else do
					hasSubGroup <- lookAhead (liftM (=="GRUP") getIdentifier >>= \g -> skip 8 >> liftM (==1) getWord32le >>= \i -> return (g && i))
					if hasSubGroup then World datasize subs <$> liftM Just get else return $ World datasize subs Nothing 
			"CELL" -> do
				empty <- isEmpty
				if empty then return $ Cell datasize subs Nothing else do
					hasSubGroup <- lookAhead (liftM (=="GRUP") getIdentifier >>= \g -> skip 8 >> liftM (==6) getWord32le >>= \i -> return (g && i))
					if hasSubGroup then Cell datasize subs <$> liftM Just get else return $ Cell datasize subs Nothing
			"DIAL" -> do
				empty <- isEmpty
				if empty then return $ Dial datasize subs Nothing else do
					hasSubGroup <- lookAhead (liftM (=="GRUP") getIdentifier >>= \g -> skip 8 >> liftM (==7) getWord32le >>= \i -> return (g && i))
					if hasSubGroup then Dial datasize subs <$> liftM Just get else return $ Dial datasize subs Nothing
			_ -> return $ Record rtype datasize subs
			--"DIAL" -> TODO-}

instance Binary SubRecord where
	put = undefined
	get = do
		tag'        <- getIdentifier
		size'       <- getWord16le
		(tag, size) <- case tag' of
			"XXXX" -> do
				size <- getWord32le
				tag  <- getIdentifier
				skip 2
				return (tag, size)
			_ -> return (tag', fromIntegral size')
		case tag of
			"XCLW" -> XCLW <$> getFloat32le
			"XCLC" -> XCLC <$> getInt32le <*> getInt32le
			"FULL" -> FULL <$> getString
			"EDID" -> EDID <$> getString
			_      -> do
				skip (fromIntegral size)
				return $ Muu tag (fromIntegral size)

getString     = liftM CL.unpack getLazyByteStringNul
getIdentifier = liftM identifier getWord32le
--getInt32le    = liftM fromIntegral getWord32le

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

main = do
	input <- BL.readFile "oblivion.esm"
	let groups = runGet (getAll) input :: [GroupOrRecord]
	let output = "<esm>" ++ concatMap show groups ++ "</esm>"-- $ (!!1) $ getChildren $ extractG $ groups!!46
	--writeFile "out2.xml" output
	lista <- runX $ readString [] output 
	--	>>> root [] [mkelem "root" [] [
		>>> getChildren 
		>>> getChildren 
		>>> hasName "top" >>> hasAttrValue "label" (=="WRLD")
		>>> getChildren	>>> hasName "record" `containing` (getChildren >>> hasName "edid" >>> hasAttrValue "id" (=="SEWorld"))
		>>> deep (isElem >>> hasName "record" `containing` (getChildren >>> hasName "xclw")) >>> (getChildren >>> hasName "xclc" >>> (getAttrValue "x" &&& getAttrValue "y")) &&& (getChildren >>> hasName "xclw" >>> getAttrValue "height")
--		>>> deep (hasName "record") >>> ((getChildren >>> hasName "xclc" >>> (getAttrValue "x" &&& getAttrValue "y")) &&& (getChildren >>> hasName "xclw" >>> getAttrValue "height"))]]
		>>> (writeDocument [withIndent yes] "out2.xml")
--	return ()
--	writeFile "out.txt" $ unlines $ map show lista
--	return ()
